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
-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckRefreshStatuses
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the refresh status of the Trusted Advisor checks that have the specified check IDs. Check IDs can be obtained by calling 'DescribeTrustedAdvisorChecks' .
--
--
module Network.AWS.Support.DescribeTrustedAdvisorCheckRefreshStatuses
    (
    -- * Creating a Request
      describeTrustedAdvisorCheckRefreshStatuses
    , DescribeTrustedAdvisorCheckRefreshStatuses
    -- * Request Lenses
    , dtacrsCheckIds

    -- * Destructuring the Response
    , describeTrustedAdvisorCheckRefreshStatusesResponse
    , DescribeTrustedAdvisorCheckRefreshStatusesResponse
    -- * Response Lenses
    , dtacrsrsResponseStatus
    , dtacrsrsStatuses
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Support.Types
import Network.AWS.Support.Types.Product

-- |
--
--
--
-- /See:/ 'describeTrustedAdvisorCheckRefreshStatuses' smart constructor.
newtype DescribeTrustedAdvisorCheckRefreshStatuses = DescribeTrustedAdvisorCheckRefreshStatuses'
  { _dtacrsCheckIds :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTrustedAdvisorCheckRefreshStatuses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtacrsCheckIds' - The IDs of the Trusted Advisor checks to get the status of. __Note:__ Specifying the check ID of a check that is automatically refreshed causes an @InvalidParameterValue@ error.
describeTrustedAdvisorCheckRefreshStatuses
    :: DescribeTrustedAdvisorCheckRefreshStatuses
describeTrustedAdvisorCheckRefreshStatuses =
  DescribeTrustedAdvisorCheckRefreshStatuses' {_dtacrsCheckIds = mempty}


-- | The IDs of the Trusted Advisor checks to get the status of. __Note:__ Specifying the check ID of a check that is automatically refreshed causes an @InvalidParameterValue@ error.
dtacrsCheckIds :: Lens' DescribeTrustedAdvisorCheckRefreshStatuses [Text]
dtacrsCheckIds = lens _dtacrsCheckIds (\ s a -> s{_dtacrsCheckIds = a}) . _Coerce

instance AWSRequest
           DescribeTrustedAdvisorCheckRefreshStatuses
         where
        type Rs DescribeTrustedAdvisorCheckRefreshStatuses =
             DescribeTrustedAdvisorCheckRefreshStatusesResponse
        request = postJSON support
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTrustedAdvisorCheckRefreshStatusesResponse'
                   <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "statuses" .!@ mempty))

instance Hashable
           DescribeTrustedAdvisorCheckRefreshStatuses
         where

instance NFData
           DescribeTrustedAdvisorCheckRefreshStatuses
         where

instance ToHeaders
           DescribeTrustedAdvisorCheckRefreshStatuses
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSSupport_20130415.DescribeTrustedAdvisorCheckRefreshStatuses"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           DescribeTrustedAdvisorCheckRefreshStatuses
         where
        toJSON
          DescribeTrustedAdvisorCheckRefreshStatuses'{..}
          = object
              (catMaybes [Just ("checkIds" .= _dtacrsCheckIds)])

instance ToPath
           DescribeTrustedAdvisorCheckRefreshStatuses
         where
        toPath = const "/"

instance ToQuery
           DescribeTrustedAdvisorCheckRefreshStatuses
         where
        toQuery = const mempty

-- | The statuses of the Trusted Advisor checks returned by the 'DescribeTrustedAdvisorCheckRefreshStatuses' operation.
--
--
--
-- /See:/ 'describeTrustedAdvisorCheckRefreshStatusesResponse' smart constructor.
data DescribeTrustedAdvisorCheckRefreshStatusesResponse = DescribeTrustedAdvisorCheckRefreshStatusesResponse'
  { _dtacrsrsResponseStatus :: !Int
  , _dtacrsrsStatuses       :: ![TrustedAdvisorCheckRefreshStatus]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTrustedAdvisorCheckRefreshStatusesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtacrsrsResponseStatus' - -- | The response status code.
--
-- * 'dtacrsrsStatuses' - The refresh status of the specified Trusted Advisor checks.
describeTrustedAdvisorCheckRefreshStatusesResponse
    :: Int -- ^ 'dtacrsrsResponseStatus'
    -> DescribeTrustedAdvisorCheckRefreshStatusesResponse
describeTrustedAdvisorCheckRefreshStatusesResponse pResponseStatus_ =
  DescribeTrustedAdvisorCheckRefreshStatusesResponse'
    {_dtacrsrsResponseStatus = pResponseStatus_, _dtacrsrsStatuses = mempty}


-- | -- | The response status code.
dtacrsrsResponseStatus :: Lens' DescribeTrustedAdvisorCheckRefreshStatusesResponse Int
dtacrsrsResponseStatus = lens _dtacrsrsResponseStatus (\ s a -> s{_dtacrsrsResponseStatus = a})

-- | The refresh status of the specified Trusted Advisor checks.
dtacrsrsStatuses :: Lens' DescribeTrustedAdvisorCheckRefreshStatusesResponse [TrustedAdvisorCheckRefreshStatus]
dtacrsrsStatuses = lens _dtacrsrsStatuses (\ s a -> s{_dtacrsrsStatuses = a}) . _Coerce

instance NFData
           DescribeTrustedAdvisorCheckRefreshStatusesResponse
         where
