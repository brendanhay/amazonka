{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeLags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the link aggregation groups (LAGs) in your account.
--
--
-- If a LAG ID is provided, only information about the specified LAG is returned.
--
module Network.AWS.DirectConnect.DescribeLags
    (
    -- * Creating a Request
      describeLags
    , DescribeLags
    -- * Request Lenses
    , dlLagId

    -- * Destructuring the Response
    , describeLagsResponse
    , DescribeLagsResponse
    -- * Response Lenses
    , desrsLags
    , desrsResponseStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the DescribeLags operation.
--
--
--
-- /See:/ 'describeLags' smart constructor.
newtype DescribeLags = DescribeLags'
  { _dlLagId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlLagId' - The ID of the LAG. Example: dxlag-abc123 Default: None
describeLags
    :: DescribeLags
describeLags = DescribeLags' {_dlLagId = Nothing}


-- | The ID of the LAG. Example: dxlag-abc123 Default: None
dlLagId :: Lens' DescribeLags (Maybe Text)
dlLagId = lens _dlLagId (\ s a -> s{_dlLagId = a})

instance AWSRequest DescribeLags where
        type Rs DescribeLags = DescribeLagsResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLagsResponse' <$>
                   (x .?> "lags" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable DescribeLags where

instance NFData DescribeLags where

instance ToHeaders DescribeLags where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DescribeLags" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeLags where
        toJSON DescribeLags'{..}
          = object (catMaybes [("lagId" .=) <$> _dlLagId])

instance ToPath DescribeLags where
        toPath = const "/"

instance ToQuery DescribeLags where
        toQuery = const mempty

-- | A structure containing a list of LAGs.
--
--
--
-- /See:/ 'describeLagsResponse' smart constructor.
data DescribeLagsResponse = DescribeLagsResponse'
  { _desrsLags           :: !(Maybe [Lag])
  , _desrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsLags' - A list of LAGs.
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeLagsResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> DescribeLagsResponse
describeLagsResponse pResponseStatus_ =
  DescribeLagsResponse'
    {_desrsLags = Nothing, _desrsResponseStatus = pResponseStatus_}


-- | A list of LAGs.
desrsLags :: Lens' DescribeLagsResponse [Lag]
desrsLags = lens _desrsLags (\ s a -> s{_desrsLags = a}) . _Default . _Coerce

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeLagsResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a})

instance NFData DescribeLagsResponse where
