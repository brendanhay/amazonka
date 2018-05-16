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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a stack's provisioning parameters.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DescribeStackProvisioningParameters
    (
    -- * Creating a Request
      describeStackProvisioningParameters
    , DescribeStackProvisioningParameters
    -- * Request Lenses
    , dsppStackId

    -- * Destructuring the Response
    , describeStackProvisioningParametersResponse
    , DescribeStackProvisioningParametersResponse
    -- * Response Lenses
    , dspprsAgentInstallerURL
    , dspprsParameters
    , dspprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeStackProvisioningParameters' smart constructor.
newtype DescribeStackProvisioningParameters = DescribeStackProvisioningParameters'
  { _dsppStackId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStackProvisioningParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsppStackId' - The stack ID
describeStackProvisioningParameters
    :: Text -- ^ 'dsppStackId'
    -> DescribeStackProvisioningParameters
describeStackProvisioningParameters pStackId_ =
  DescribeStackProvisioningParameters' {_dsppStackId = pStackId_}


-- | The stack ID
dsppStackId :: Lens' DescribeStackProvisioningParameters Text
dsppStackId = lens _dsppStackId (\ s a -> s{_dsppStackId = a})

instance AWSRequest
           DescribeStackProvisioningParameters
         where
        type Rs DescribeStackProvisioningParameters =
             DescribeStackProvisioningParametersResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 DescribeStackProvisioningParametersResponse' <$>
                   (x .?> "AgentInstallerUrl") <*>
                     (x .?> "Parameters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeStackProvisioningParameters
         where

instance NFData DescribeStackProvisioningParameters
         where

instance ToHeaders
           DescribeStackProvisioningParameters
         where
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
          = object
              (catMaybes [Just ("StackId" .= _dsppStackId)])

instance ToPath DescribeStackProvisioningParameters
         where
        toPath = const "/"

instance ToQuery DescribeStackProvisioningParameters
         where
        toQuery = const mempty

-- | Contains the response to a @DescribeStackProvisioningParameters@ request.
--
--
--
-- /See:/ 'describeStackProvisioningParametersResponse' smart constructor.
data DescribeStackProvisioningParametersResponse = DescribeStackProvisioningParametersResponse'
  { _dspprsAgentInstallerURL :: !(Maybe Text)
  , _dspprsParameters        :: !(Maybe (Map Text Text))
  , _dspprsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStackProvisioningParametersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dspprsAgentInstallerURL' - The AWS OpsWorks Stacks agent installer's URL.
--
-- * 'dspprsParameters' - An embedded object that contains the provisioning parameters.
--
-- * 'dspprsResponseStatus' - -- | The response status code.
describeStackProvisioningParametersResponse
    :: Int -- ^ 'dspprsResponseStatus'
    -> DescribeStackProvisioningParametersResponse
describeStackProvisioningParametersResponse pResponseStatus_ =
  DescribeStackProvisioningParametersResponse'
    { _dspprsAgentInstallerURL = Nothing
    , _dspprsParameters = Nothing
    , _dspprsResponseStatus = pResponseStatus_
    }


-- | The AWS OpsWorks Stacks agent installer's URL.
dspprsAgentInstallerURL :: Lens' DescribeStackProvisioningParametersResponse (Maybe Text)
dspprsAgentInstallerURL = lens _dspprsAgentInstallerURL (\ s a -> s{_dspprsAgentInstallerURL = a})

-- | An embedded object that contains the provisioning parameters.
dspprsParameters :: Lens' DescribeStackProvisioningParametersResponse (HashMap Text Text)
dspprsParameters = lens _dspprsParameters (\ s a -> s{_dspprsParameters = a}) . _Default . _Map

-- | -- | The response status code.
dspprsResponseStatus :: Lens' DescribeStackProvisioningParametersResponse Int
dspprsResponseStatus = lens _dspprsResponseStatus (\ s a -> s{_dspprsResponseStatus = a})

instance NFData
           DescribeStackProvisioningParametersResponse
         where
