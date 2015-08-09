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
-- Module      : Network.AWS.Support.RefreshTrustedAdvisorCheck
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a refresh of the Trusted Advisor check that has the specified
-- check ID. Check IDs can be obtained by calling
-- DescribeTrustedAdvisorChecks.
--
-- The response contains a TrustedAdvisorCheckRefreshStatus object, which
-- contains these fields:
--
-- -   __Status.__ The refresh status of the check: \"none\", \"enqueued\",
--     \"processing\", \"success\", or \"abandoned\".
-- -   __MillisUntilNextRefreshable.__ The amount of time, in milliseconds,
--     until the check is eligible for refresh.
-- -   __CheckId.__ The unique identifier for the check.
--
-- /See:/ <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_RefreshTrustedAdvisorCheck.html AWS API Reference> for RefreshTrustedAdvisorCheck.
module Network.AWS.Support.RefreshTrustedAdvisorCheck
    (
    -- * Creating a Request
      RefreshTrustedAdvisorCheck
    , refreshTrustedAdvisorCheck
    -- * Request Lenses
    , rtacCheckId

    -- * Destructuring the Response
    , RefreshTrustedAdvisorCheckResponse
    , refreshTrustedAdvisorCheckResponse
    -- * Response Lenses
    , rtacrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Support.Types
import           Network.AWS.Support.Types.Product

-- | /See:/ 'refreshTrustedAdvisorCheck' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtacCheckId'
newtype RefreshTrustedAdvisorCheck = RefreshTrustedAdvisorCheck'
    { _rtacCheckId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RefreshTrustedAdvisorCheck' smart constructor.
refreshTrustedAdvisorCheck :: Text -> RefreshTrustedAdvisorCheck
refreshTrustedAdvisorCheck pCheckId_ =
    RefreshTrustedAdvisorCheck'
    { _rtacCheckId = pCheckId_
    }

-- | The unique identifier for the Trusted Advisor check.
rtacCheckId :: Lens' RefreshTrustedAdvisorCheck Text
rtacCheckId = lens _rtacCheckId (\ s a -> s{_rtacCheckId = a});

instance AWSRequest RefreshTrustedAdvisorCheck where
        type Sv RefreshTrustedAdvisorCheck = Support
        type Rs RefreshTrustedAdvisorCheck =
             RefreshTrustedAdvisorCheckResponse
        request = postJSON
        response
          = receiveEmpty
              (\ s h x ->
                 RefreshTrustedAdvisorCheckResponse' <$>
                   (pure (fromEnum s)))

instance ToHeaders RefreshTrustedAdvisorCheck where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSSupport_20130415.RefreshTrustedAdvisorCheck" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RefreshTrustedAdvisorCheck where
        toJSON RefreshTrustedAdvisorCheck'{..}
          = object ["checkId" .= _rtacCheckId]

instance ToPath RefreshTrustedAdvisorCheck where
        toPath = const "/"

instance ToQuery RefreshTrustedAdvisorCheck where
        toQuery = const mempty

-- | The current refresh status of a Trusted Advisor check.
--
-- /See:/ 'refreshTrustedAdvisorCheckResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtacrsStatus'
newtype RefreshTrustedAdvisorCheckResponse = RefreshTrustedAdvisorCheckResponse'
    { _rtacrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RefreshTrustedAdvisorCheckResponse' smart constructor.
refreshTrustedAdvisorCheckResponse :: Int -> RefreshTrustedAdvisorCheckResponse
refreshTrustedAdvisorCheckResponse pStatus_ =
    RefreshTrustedAdvisorCheckResponse'
    { _rtacrsStatus = pStatus_
    }

-- | Undocumented member.
rtacrsStatus :: Lens' RefreshTrustedAdvisorCheckResponse Int
rtacrsStatus = lens _rtacrsStatus (\ s a -> s{_rtacrsStatus = a});
