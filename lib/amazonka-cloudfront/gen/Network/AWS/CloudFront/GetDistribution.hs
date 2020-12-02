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
-- Module      : Network.AWS.CloudFront.GetDistribution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the information about a distribution.
--
--
module Network.AWS.CloudFront.GetDistribution
    (
    -- * Creating a Request
      getDistribution
    , GetDistribution
    -- * Request Lenses
    , gdId

    -- * Destructuring the Response
    , getDistributionResponse
    , GetDistributionResponse
    -- * Response Lenses
    , gdrsETag
    , gdrsDistribution
    , gdrsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to get a distribution's information.
--
--
--
-- /See:/ 'getDistribution' smart constructor.
newtype GetDistribution = GetDistribution'
  { _gdId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdId' - The distribution's ID.
getDistribution
    :: Text -- ^ 'gdId'
    -> GetDistribution
getDistribution pId_ = GetDistribution' {_gdId = pId_}


-- | The distribution's ID.
gdId :: Lens' GetDistribution Text
gdId = lens _gdId (\ s a -> s{_gdId = a})

instance AWSRequest GetDistribution where
        type Rs GetDistribution = GetDistributionResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 GetDistributionResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (pure (fromEnum s)))

instance Hashable GetDistribution where

instance NFData GetDistribution where

instance ToHeaders GetDistribution where
        toHeaders = const mempty

instance ToPath GetDistribution where
        toPath GetDistribution'{..}
          = mconcat ["/2017-10-30/distribution/", toBS _gdId]

instance ToQuery GetDistribution where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
--
--
-- /See:/ 'getDistributionResponse' smart constructor.
data GetDistributionResponse = GetDistributionResponse'
  { _gdrsETag           :: !(Maybe Text)
  , _gdrsDistribution   :: !(Maybe Distribution)
  , _gdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDistributionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsETag' - The current version of the distribution's information. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'gdrsDistribution' - The distribution's information.
--
-- * 'gdrsResponseStatus' - -- | The response status code.
getDistributionResponse
    :: Int -- ^ 'gdrsResponseStatus'
    -> GetDistributionResponse
getDistributionResponse pResponseStatus_ =
  GetDistributionResponse'
    { _gdrsETag = Nothing
    , _gdrsDistribution = Nothing
    , _gdrsResponseStatus = pResponseStatus_
    }


-- | The current version of the distribution's information. For example: @E2QWRUHAPOMQZL@ .
gdrsETag :: Lens' GetDistributionResponse (Maybe Text)
gdrsETag = lens _gdrsETag (\ s a -> s{_gdrsETag = a})

-- | The distribution's information.
gdrsDistribution :: Lens' GetDistributionResponse (Maybe Distribution)
gdrsDistribution = lens _gdrsDistribution (\ s a -> s{_gdrsDistribution = a})

-- | -- | The response status code.
gdrsResponseStatus :: Lens' GetDistributionResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\ s a -> s{_gdrsResponseStatus = a})

instance NFData GetDistributionResponse where
