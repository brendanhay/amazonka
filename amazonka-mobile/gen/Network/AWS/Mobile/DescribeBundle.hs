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
-- Module      : Network.AWS.Mobile.DescribeBundle
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the bundle details for the requested bundle id.
--
--
module Network.AWS.Mobile.DescribeBundle
    (
    -- * Creating a Request
      describeBundle
    , DescribeBundle
    -- * Request Lenses
    , dbBundleId

    -- * Destructuring the Response
    , describeBundleResponse
    , DescribeBundleResponse
    -- * Response Lenses
    , dbrsDetails
    , dbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Mobile.Types
import Network.AWS.Mobile.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure to request the details of a specific bundle.
--
--
--
-- /See:/ 'describeBundle' smart constructor.
newtype DescribeBundle = DescribeBundle'
  { _dbBundleId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBundle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbBundleId' - Unique bundle identifier.
describeBundle
    :: Text -- ^ 'dbBundleId'
    -> DescribeBundle
describeBundle pBundleId_ = DescribeBundle' {_dbBundleId = pBundleId_}


-- | Unique bundle identifier.
dbBundleId :: Lens' DescribeBundle Text
dbBundleId = lens _dbBundleId (\ s a -> s{_dbBundleId = a})

instance AWSRequest DescribeBundle where
        type Rs DescribeBundle = DescribeBundleResponse
        request = get mobile
        response
          = receiveJSON
              (\ s h x ->
                 DescribeBundleResponse' <$>
                   (x .?> "details") <*> (pure (fromEnum s)))

instance Hashable DescribeBundle where

instance NFData DescribeBundle where

instance ToHeaders DescribeBundle where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeBundle where
        toPath DescribeBundle'{..}
          = mconcat ["/bundles/", toBS _dbBundleId]

instance ToQuery DescribeBundle where
        toQuery = const mempty

-- | Result structure contains the details of the bundle.
--
--
--
-- /See:/ 'describeBundleResponse' smart constructor.
data DescribeBundleResponse = DescribeBundleResponse'
  { _dbrsDetails        :: !(Maybe BundleDetails)
  , _dbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBundleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbrsDetails' - The details of the bundle.
--
-- * 'dbrsResponseStatus' - -- | The response status code.
describeBundleResponse
    :: Int -- ^ 'dbrsResponseStatus'
    -> DescribeBundleResponse
describeBundleResponse pResponseStatus_ =
  DescribeBundleResponse'
    {_dbrsDetails = Nothing, _dbrsResponseStatus = pResponseStatus_}


-- | The details of the bundle.
dbrsDetails :: Lens' DescribeBundleResponse (Maybe BundleDetails)
dbrsDetails = lens _dbrsDetails (\ s a -> s{_dbrsDetails = a})

-- | -- | The response status code.
dbrsResponseStatus :: Lens' DescribeBundleResponse Int
dbrsResponseStatus = lens _dbrsResponseStatus (\ s a -> s{_dbrsResponseStatus = a})

instance NFData DescribeBundleResponse where
