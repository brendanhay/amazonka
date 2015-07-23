{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeStackSummary
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the number of layers and apps in a specified stack, and the
-- number of instances in each state, such as @running_setup@ or @online@.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeStackSummary.html>
module Network.AWS.OpsWorks.DescribeStackSummary
    (
    -- * Request
      DescribeStackSummary
    -- ** Request constructor
    , describeStackSummary
    -- ** Request lenses
    , dssrqStackId

    -- * Response
    , DescribeStackSummaryResponse
    -- ** Response constructor
    , describeStackSummaryResponse
    -- ** Response lenses
    , dssrsStackSummary
    , dssrsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeStackSummary' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dssrqStackId'
newtype DescribeStackSummary = DescribeStackSummary'
    { _dssrqStackId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeStackSummary' smart constructor.
describeStackSummary :: Text -> DescribeStackSummary
describeStackSummary pStackId_ =
    DescribeStackSummary'
    { _dssrqStackId = pStackId_
    }

-- | The stack ID.
dssrqStackId :: Lens' DescribeStackSummary Text
dssrqStackId = lens _dssrqStackId (\ s a -> s{_dssrqStackId = a});

instance AWSRequest DescribeStackSummary where
        type Sv DescribeStackSummary = OpsWorks
        type Rs DescribeStackSummary =
             DescribeStackSummaryResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeStackSummaryResponse' <$>
                   (x .?> "StackSummary") <*> (pure (fromEnum s)))

instance ToHeaders DescribeStackSummary where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeStackSummary" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeStackSummary where
        toJSON DescribeStackSummary'{..}
          = object ["StackId" .= _dssrqStackId]

instance ToPath DescribeStackSummary where
        toPath = const "/"

instance ToQuery DescribeStackSummary where
        toQuery = const mempty

-- | Contains the response to a @DescribeStackSummary@ request.
--
-- /See:/ 'describeStackSummaryResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dssrsStackSummary'
--
-- * 'dssrsStatus'
data DescribeStackSummaryResponse = DescribeStackSummaryResponse'
    { _dssrsStackSummary :: !(Maybe StackSummary)
    , _dssrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeStackSummaryResponse' smart constructor.
describeStackSummaryResponse :: Int -> DescribeStackSummaryResponse
describeStackSummaryResponse pStatus_ =
    DescribeStackSummaryResponse'
    { _dssrsStackSummary = Nothing
    , _dssrsStatus = pStatus_
    }

-- | A @StackSummary@ object that contains the results.
dssrsStackSummary :: Lens' DescribeStackSummaryResponse (Maybe StackSummary)
dssrsStackSummary = lens _dssrsStackSummary (\ s a -> s{_dssrsStackSummary = a});

-- | FIXME: Undocumented member.
dssrsStatus :: Lens' DescribeStackSummaryResponse Int
dssrsStatus = lens _dssrsStatus (\ s a -> s{_dssrsStatus = a});
