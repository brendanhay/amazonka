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
-- Module      : Network.AWS.Shield.DescribeProtection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the details of a 'Protection' object.
--
--
module Network.AWS.Shield.DescribeProtection
    (
    -- * Creating a Request
      describeProtection
    , DescribeProtection
    -- * Request Lenses
    , dpProtectionId

    -- * Destructuring the Response
    , describeProtectionResponse
    , DescribeProtectionResponse
    -- * Response Lenses
    , dprsProtection
    , dprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types
import Network.AWS.Shield.Types.Product

-- | /See:/ 'describeProtection' smart constructor.
newtype DescribeProtection = DescribeProtection'
  { _dpProtectionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeProtection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpProtectionId' - The unique identifier (ID) for the 'Protection' object that is described.
describeProtection
    :: Text -- ^ 'dpProtectionId'
    -> DescribeProtection
describeProtection pProtectionId_ =
  DescribeProtection' {_dpProtectionId = pProtectionId_}


-- | The unique identifier (ID) for the 'Protection' object that is described.
dpProtectionId :: Lens' DescribeProtection Text
dpProtectionId = lens _dpProtectionId (\ s a -> s{_dpProtectionId = a})

instance AWSRequest DescribeProtection where
        type Rs DescribeProtection =
             DescribeProtectionResponse
        request = postJSON shield
        response
          = receiveJSON
              (\ s h x ->
                 DescribeProtectionResponse' <$>
                   (x .?> "Protection") <*> (pure (fromEnum s)))

instance Hashable DescribeProtection where

instance NFData DescribeProtection where

instance ToHeaders DescribeProtection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSShield_20160616.DescribeProtection" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeProtection where
        toJSON DescribeProtection'{..}
          = object
              (catMaybes
                 [Just ("ProtectionId" .= _dpProtectionId)])

instance ToPath DescribeProtection where
        toPath = const "/"

instance ToQuery DescribeProtection where
        toQuery = const mempty

-- | /See:/ 'describeProtectionResponse' smart constructor.
data DescribeProtectionResponse = DescribeProtectionResponse'
  { _dprsProtection     :: !(Maybe Protection)
  , _dprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeProtectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprsProtection' - The 'Protection' object that is described.
--
-- * 'dprsResponseStatus' - -- | The response status code.
describeProtectionResponse
    :: Int -- ^ 'dprsResponseStatus'
    -> DescribeProtectionResponse
describeProtectionResponse pResponseStatus_ =
  DescribeProtectionResponse'
    {_dprsProtection = Nothing, _dprsResponseStatus = pResponseStatus_}


-- | The 'Protection' object that is described.
dprsProtection :: Lens' DescribeProtectionResponse (Maybe Protection)
dprsProtection = lens _dprsProtection (\ s a -> s{_dprsProtection = a})

-- | -- | The response status code.
dprsResponseStatus :: Lens' DescribeProtectionResponse Int
dprsResponseStatus = lens _dprsResponseStatus (\ s a -> s{_dprsResponseStatus = a})

instance NFData DescribeProtectionResponse where
