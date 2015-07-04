{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.OpsWorks.DescribeStackProvisioningParameters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Requests a description of a stack\'s provisioning parameters.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeStackProvisioningParameters.html>
module Network.AWS.OpsWorks.DescribeStackProvisioningParameters
    (
    -- * Request
      DescribeStackProvisioningParameters
    -- ** Request constructor
    , describeStackProvisioningParameters
    -- ** Request lenses
    , dsppStackId

    -- * Response
    , DescribeStackProvisioningParametersResponse
    -- ** Response constructor
    , describeStackProvisioningParametersResponse
    -- ** Response lenses
    , dspprAgentInstallerURL
    , dspprParameters
    , dspprStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeStackProvisioningParameters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsppStackId'
newtype DescribeStackProvisioningParameters = DescribeStackProvisioningParameters'
    { _dsppStackId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeStackProvisioningParameters' smart constructor.
describeStackProvisioningParameters :: Text -> DescribeStackProvisioningParameters
describeStackProvisioningParameters pStackId =
    DescribeStackProvisioningParameters'
    { _dsppStackId = pStackId
    }

-- | The stack ID
dsppStackId :: Lens' DescribeStackProvisioningParameters Text
dsppStackId = lens _dsppStackId (\ s a -> s{_dsppStackId = a});

instance AWSRequest
         DescribeStackProvisioningParameters where
        type Sv DescribeStackProvisioningParameters =
             OpsWorks
        type Rs DescribeStackProvisioningParameters =
             DescribeStackProvisioningParametersResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeStackProvisioningParametersResponse' <$>
                   (x .?> "AgentInstallerUrl") <*>
                     (x .?> "Parameters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders
         DescribeStackProvisioningParameters where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeStackProvisioningParameters"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeStackProvisioningParameters
         where
        toJSON DescribeStackProvisioningParameters'{..}
          = object ["StackId" .= _dsppStackId]

instance ToPath DescribeStackProvisioningParameters
         where
        toPath = const "/"

instance ToQuery DescribeStackProvisioningParameters
         where
        toQuery = const mempty

-- | Contains the response to a @DescribeStackProvisioningParameters@
-- request.
--
-- /See:/ 'describeStackProvisioningParametersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dspprAgentInstallerURL'
--
-- * 'dspprParameters'
--
-- * 'dspprStatus'
data DescribeStackProvisioningParametersResponse = DescribeStackProvisioningParametersResponse'
    { _dspprAgentInstallerURL :: !(Maybe Text)
    , _dspprParameters        :: !(Maybe (Map Text Text))
    , _dspprStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeStackProvisioningParametersResponse' smart constructor.
describeStackProvisioningParametersResponse :: Int -> DescribeStackProvisioningParametersResponse
describeStackProvisioningParametersResponse pStatus =
    DescribeStackProvisioningParametersResponse'
    { _dspprAgentInstallerURL = Nothing
    , _dspprParameters = Nothing
    , _dspprStatus = pStatus
    }

-- | The AWS OpsWorks agent installer\'s URL.
dspprAgentInstallerURL :: Lens' DescribeStackProvisioningParametersResponse (Maybe Text)
dspprAgentInstallerURL = lens _dspprAgentInstallerURL (\ s a -> s{_dspprAgentInstallerURL = a});

-- | An embedded object that contains the provisioning parameters.
dspprParameters :: Lens' DescribeStackProvisioningParametersResponse (HashMap Text Text)
dspprParameters = lens _dspprParameters (\ s a -> s{_dspprParameters = a}) . _Default . _Map;

-- | FIXME: Undocumented member.
dspprStatus :: Lens' DescribeStackProvisioningParametersResponse Int
dspprStatus = lens _dspprStatus (\ s a -> s{_dspprStatus = a});
