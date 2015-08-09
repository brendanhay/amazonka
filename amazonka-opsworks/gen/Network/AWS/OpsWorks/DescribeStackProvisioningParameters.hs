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
-- Module      : Network.AWS.OpsWorks.DescribeStackProvisioningParameters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a stack\'s provisioning parameters.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- /See:/ <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeStackProvisioningParameters.html AWS API Reference> for DescribeStackProvisioningParameters.
module Network.AWS.OpsWorks.DescribeStackProvisioningParameters
    (
    -- * Creating a Request
      DescribeStackProvisioningParameters
    , describeStackProvisioningParameters
    -- * Request Lenses
    , dsppStackId

    -- * Destructuring the Response
    , DescribeStackProvisioningParametersResponse
    , describeStackProvisioningParametersResponse
    -- * Response Lenses
    , dspprsAgentInstallerURL
    , dspprsParameters
    , dspprsStatus
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

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
describeStackProvisioningParameters pStackId_ = 
    DescribeStackProvisioningParameters'
    { _dsppStackId = pStackId_
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
-- * 'dspprsAgentInstallerURL'
--
-- * 'dspprsParameters'
--
-- * 'dspprsStatus'
data DescribeStackProvisioningParametersResponse = DescribeStackProvisioningParametersResponse'
    { _dspprsAgentInstallerURL :: !(Maybe Text)
    , _dspprsParameters :: !(Maybe (Map Text Text))
    , _dspprsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeStackProvisioningParametersResponse' smart constructor.
describeStackProvisioningParametersResponse :: Int -> DescribeStackProvisioningParametersResponse
describeStackProvisioningParametersResponse pStatus_ = 
    DescribeStackProvisioningParametersResponse'
    { _dspprsAgentInstallerURL = Nothing
    , _dspprsParameters = Nothing
    , _dspprsStatus = pStatus_
    }

-- | The AWS OpsWorks agent installer\'s URL.
dspprsAgentInstallerURL :: Lens' DescribeStackProvisioningParametersResponse (Maybe Text)
dspprsAgentInstallerURL = lens _dspprsAgentInstallerURL (\ s a -> s{_dspprsAgentInstallerURL = a});

-- | An embedded object that contains the provisioning parameters.
dspprsParameters :: Lens' DescribeStackProvisioningParametersResponse (HashMap Text Text)
dspprsParameters = lens _dspprsParameters (\ s a -> s{_dspprsParameters = a}) . _Default . _Map;

-- | Undocumented member.
dspprsStatus :: Lens' DescribeStackProvisioningParametersResponse Int
dspprsStatus = lens _dspprsStatus (\ s a -> s{_dspprsStatus = a});
