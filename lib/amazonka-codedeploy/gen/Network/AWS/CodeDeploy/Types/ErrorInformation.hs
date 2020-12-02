{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.ErrorInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ErrorInformation where

import Network.AWS.CodeDeploy.Types.DeployErrorCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a deployment error.
--
--
--
-- /See:/ 'errorInformation' smart constructor.
data ErrorInformation = ErrorInformation'
  { _eiCode ::
      !(Maybe DeployErrorCode),
    _eiMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ErrorInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiCode' - For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/error-codes.html Error Codes for AWS CodeDeploy> in the <https://docs.aws.amazon.com/codedeploy/latest/userguide AWS CodeDeploy User Guide> . The error code:     * APPLICATION_MISSING: The application was missing. This error code is most likely raised if the application is deleted after the deployment is created, but before it is started.     * DEPLOYMENT_GROUP_MISSING: The deployment group was missing. This error code is most likely raised if the deployment group is deleted after the deployment is created, but before it is started.     * HEALTH_CONSTRAINTS: The deployment failed on too many instances to be successfully deployed within the instance health constraints specified.     * HEALTH_CONSTRAINTS_INVALID: The revision cannot be successfully deployed within the instance health constraints specified.     * IAM_ROLE_MISSING: The service role cannot be accessed.     * IAM_ROLE_PERMISSIONS: The service role does not have the correct permissions.     * INTERNAL_ERROR: There was an internal error.     * NO_EC2_SUBSCRIPTION: The calling account is not subscribed to Amazon EC2.     * NO_INSTANCES: No instances were specified, or no instances can be found.     * OVER_MAX_INSTANCES: The maximum number of instances was exceeded.     * THROTTLED: The operation was throttled because the calling account exceeded the throttling limits of one or more AWS services.     * TIMEOUT: The deployment has timed out.     * REVISION_MISSING: The revision ID was missing. This error code is most likely raised if the revision is deleted after the deployment is created, but before it is started.
--
-- * 'eiMessage' - An accompanying error message.
errorInformation ::
  ErrorInformation
errorInformation =
  ErrorInformation' {_eiCode = Nothing, _eiMessage = Nothing}

-- | For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/error-codes.html Error Codes for AWS CodeDeploy> in the <https://docs.aws.amazon.com/codedeploy/latest/userguide AWS CodeDeploy User Guide> . The error code:     * APPLICATION_MISSING: The application was missing. This error code is most likely raised if the application is deleted after the deployment is created, but before it is started.     * DEPLOYMENT_GROUP_MISSING: The deployment group was missing. This error code is most likely raised if the deployment group is deleted after the deployment is created, but before it is started.     * HEALTH_CONSTRAINTS: The deployment failed on too many instances to be successfully deployed within the instance health constraints specified.     * HEALTH_CONSTRAINTS_INVALID: The revision cannot be successfully deployed within the instance health constraints specified.     * IAM_ROLE_MISSING: The service role cannot be accessed.     * IAM_ROLE_PERMISSIONS: The service role does not have the correct permissions.     * INTERNAL_ERROR: There was an internal error.     * NO_EC2_SUBSCRIPTION: The calling account is not subscribed to Amazon EC2.     * NO_INSTANCES: No instances were specified, or no instances can be found.     * OVER_MAX_INSTANCES: The maximum number of instances was exceeded.     * THROTTLED: The operation was throttled because the calling account exceeded the throttling limits of one or more AWS services.     * TIMEOUT: The deployment has timed out.     * REVISION_MISSING: The revision ID was missing. This error code is most likely raised if the revision is deleted after the deployment is created, but before it is started.
eiCode :: Lens' ErrorInformation (Maybe DeployErrorCode)
eiCode = lens _eiCode (\s a -> s {_eiCode = a})

-- | An accompanying error message.
eiMessage :: Lens' ErrorInformation (Maybe Text)
eiMessage = lens _eiMessage (\s a -> s {_eiMessage = a})

instance FromJSON ErrorInformation where
  parseJSON =
    withObject
      "ErrorInformation"
      (\x -> ErrorInformation' <$> (x .:? "code") <*> (x .:? "message"))

instance Hashable ErrorInformation

instance NFData ErrorInformation
