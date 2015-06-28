{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.DescribeStackSummary
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes the number of layers and apps in a specified stack, and the
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
    , dssStackId

    -- * Response
    , DescribeStackSummaryResponse
    -- ** Response constructor
    , describeStackSummaryResponse
    -- ** Response lenses
    , dssrStackSummary
    , dssrStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeStackSummary' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dssStackId'
newtype DescribeStackSummary = DescribeStackSummary'
    { _dssStackId :: Text
    } deriving (Eq,Read,Show)

-- | 'DescribeStackSummary' smart constructor.
describeStackSummary :: Text -> DescribeStackSummary
describeStackSummary pStackId =
    DescribeStackSummary'
    { _dssStackId = pStackId
    }

-- | The stack ID.
dssStackId :: Lens' DescribeStackSummary Text
dssStackId = lens _dssStackId (\ s a -> s{_dssStackId = a});

instance AWSRequest DescribeStackSummary where
        type Sv DescribeStackSummary = OpsWorks
        type Rs DescribeStackSummary =
             DescribeStackSummaryResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeStackSummaryResponse' <$>
                   (x .?> "StackSummary") <*> (pure s))

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
          = object ["StackId" .= _dssStackId]

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
-- * 'dssrStackSummary'
--
-- * 'dssrStatus'
data DescribeStackSummaryResponse = DescribeStackSummaryResponse'
    { _dssrStackSummary :: !(Maybe StackSummary)
    , _dssrStatus       :: !Status
    } deriving (Eq,Read,Show)

-- | 'DescribeStackSummaryResponse' smart constructor.
describeStackSummaryResponse :: Status -> DescribeStackSummaryResponse
describeStackSummaryResponse pStatus =
    DescribeStackSummaryResponse'
    { _dssrStackSummary = Nothing
    , _dssrStatus = pStatus
    }

-- | A @StackSummary@ object that contains the results.
dssrStackSummary :: Lens' DescribeStackSummaryResponse (Maybe StackSummary)
dssrStackSummary = lens _dssrStackSummary (\ s a -> s{_dssrStackSummary = a});

-- | FIXME: Undocumented member.
dssrStatus :: Lens' DescribeStackSummaryResponse Status
dssrStatus = lens _dssrStatus (\ s a -> s{_dssrStatus = a});
