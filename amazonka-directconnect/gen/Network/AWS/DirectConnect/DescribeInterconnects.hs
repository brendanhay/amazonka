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
-- Module      : Network.AWS.DirectConnect.DescribeInterconnects
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of interconnects owned by the AWS account.
--
--
-- If an interconnect ID is provided, it will only return this particular interconnect.
--
module Network.AWS.DirectConnect.DescribeInterconnects
    (
    -- * Creating a Request
      describeInterconnects
    , DescribeInterconnects
    -- * Request Lenses
    , diInterconnectId

    -- * Destructuring the Response
    , describeInterconnectsResponse
    , DescribeInterconnectsResponse
    -- * Response Lenses
    , dirsInterconnects
    , dirsResponseStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the DescribeInterconnects operation.
--
--
--
-- /See:/ 'describeInterconnects' smart constructor.
newtype DescribeInterconnects = DescribeInterconnects'
  { _diInterconnectId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInterconnects' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diInterconnectId' - Undocumented member.
describeInterconnects
    :: DescribeInterconnects
describeInterconnects = DescribeInterconnects' {_diInterconnectId = Nothing}


-- | Undocumented member.
diInterconnectId :: Lens' DescribeInterconnects (Maybe Text)
diInterconnectId = lens _diInterconnectId (\ s a -> s{_diInterconnectId = a})

instance AWSRequest DescribeInterconnects where
        type Rs DescribeInterconnects =
             DescribeInterconnectsResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 DescribeInterconnectsResponse' <$>
                   (x .?> "interconnects" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeInterconnects where

instance NFData DescribeInterconnects where

instance ToHeaders DescribeInterconnects where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DescribeInterconnects" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeInterconnects where
        toJSON DescribeInterconnects'{..}
          = object
              (catMaybes
                 [("interconnectId" .=) <$> _diInterconnectId])

instance ToPath DescribeInterconnects where
        toPath = const "/"

instance ToQuery DescribeInterconnects where
        toQuery = const mempty

-- | A structure containing a list of interconnects.
--
--
--
-- /See:/ 'describeInterconnectsResponse' smart constructor.
data DescribeInterconnectsResponse = DescribeInterconnectsResponse'
  { _dirsInterconnects  :: !(Maybe [Interconnect])
  , _dirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInterconnectsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsInterconnects' - A list of interconnects.
--
-- * 'dirsResponseStatus' - -- | The response status code.
describeInterconnectsResponse
    :: Int -- ^ 'dirsResponseStatus'
    -> DescribeInterconnectsResponse
describeInterconnectsResponse pResponseStatus_ =
  DescribeInterconnectsResponse'
    {_dirsInterconnects = Nothing, _dirsResponseStatus = pResponseStatus_}


-- | A list of interconnects.
dirsInterconnects :: Lens' DescribeInterconnectsResponse [Interconnect]
dirsInterconnects = lens _dirsInterconnects (\ s a -> s{_dirsInterconnects = a}) . _Default . _Coerce

-- | -- | The response status code.
dirsResponseStatus :: Lens' DescribeInterconnectsResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\ s a -> s{_dirsResponseStatus = a})

instance NFData DescribeInterconnectsResponse where
