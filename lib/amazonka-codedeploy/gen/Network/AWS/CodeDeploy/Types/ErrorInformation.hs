-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.ErrorInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ErrorInformation
  ( ErrorInformation (..),

    -- * Smart constructor
    mkErrorInformation,

    -- * Lenses
    eiCode,
    eiMessage,
  )
where

import Network.AWS.CodeDeploy.Types.DeployErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a deployment error.
--
-- /See:/ 'mkErrorInformation' smart constructor.
data ErrorInformation = ErrorInformation'
  { code ::
      Lude.Maybe DeployErrorCode,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ErrorInformation' with the minimum fields required to make a request.
--
-- * 'code' - For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/error-codes.html Error Codes for AWS CodeDeploy> in the <https://docs.aws.amazon.com/codedeploy/latest/userguide AWS CodeDeploy User Guide> .
--
-- The error code:
--
--     * APPLICATION_MISSING: The application was missing. This error code is most likely raised if the application is deleted after the deployment is created, but before it is started.
--
--
--     * DEPLOYMENT_GROUP_MISSING: The deployment group was missing. This error code is most likely raised if the deployment group is deleted after the deployment is created, but before it is started.
--
--
--     * HEALTH_CONSTRAINTS: The deployment failed on too many instances to be successfully deployed within the instance health constraints specified.
--
--
--     * HEALTH_CONSTRAINTS_INVALID: The revision cannot be successfully deployed within the instance health constraints specified.
--
--
--     * IAM_ROLE_MISSING: The service role cannot be accessed.
--
--
--     * IAM_ROLE_PERMISSIONS: The service role does not have the correct permissions.
--
--
--     * INTERNAL_ERROR: There was an internal error.
--
--
--     * NO_EC2_SUBSCRIPTION: The calling account is not subscribed to Amazon EC2.
--
--
--     * NO_INSTANCES: No instances were specified, or no instances can be found.
--
--
--     * OVER_MAX_INSTANCES: The maximum number of instances was exceeded.
--
--
--     * THROTTLED: The operation was throttled because the calling account exceeded the throttling limits of one or more AWS services.
--
--
--     * TIMEOUT: The deployment has timed out.
--
--
--     * REVISION_MISSING: The revision ID was missing. This error code is most likely raised if the revision is deleted after the deployment is created, but before it is started.
--
--
-- * 'message' - An accompanying error message.
mkErrorInformation ::
  ErrorInformation
mkErrorInformation =
  ErrorInformation' {code = Lude.Nothing, message = Lude.Nothing}

-- | For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/error-codes.html Error Codes for AWS CodeDeploy> in the <https://docs.aws.amazon.com/codedeploy/latest/userguide AWS CodeDeploy User Guide> .
--
-- The error code:
--
--     * APPLICATION_MISSING: The application was missing. This error code is most likely raised if the application is deleted after the deployment is created, but before it is started.
--
--
--     * DEPLOYMENT_GROUP_MISSING: The deployment group was missing. This error code is most likely raised if the deployment group is deleted after the deployment is created, but before it is started.
--
--
--     * HEALTH_CONSTRAINTS: The deployment failed on too many instances to be successfully deployed within the instance health constraints specified.
--
--
--     * HEALTH_CONSTRAINTS_INVALID: The revision cannot be successfully deployed within the instance health constraints specified.
--
--
--     * IAM_ROLE_MISSING: The service role cannot be accessed.
--
--
--     * IAM_ROLE_PERMISSIONS: The service role does not have the correct permissions.
--
--
--     * INTERNAL_ERROR: There was an internal error.
--
--
--     * NO_EC2_SUBSCRIPTION: The calling account is not subscribed to Amazon EC2.
--
--
--     * NO_INSTANCES: No instances were specified, or no instances can be found.
--
--
--     * OVER_MAX_INSTANCES: The maximum number of instances was exceeded.
--
--
--     * THROTTLED: The operation was throttled because the calling account exceeded the throttling limits of one or more AWS services.
--
--
--     * TIMEOUT: The deployment has timed out.
--
--
--     * REVISION_MISSING: The revision ID was missing. This error code is most likely raised if the revision is deleted after the deployment is created, but before it is started.
--
--
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiCode :: Lens.Lens' ErrorInformation (Lude.Maybe DeployErrorCode)
eiCode = Lens.lens (code :: ErrorInformation -> Lude.Maybe DeployErrorCode) (\s a -> s {code = a} :: ErrorInformation)
{-# DEPRECATED eiCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | An accompanying error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiMessage :: Lens.Lens' ErrorInformation (Lude.Maybe Lude.Text)
eiMessage = Lens.lens (message :: ErrorInformation -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ErrorInformation)
{-# DEPRECATED eiMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON ErrorInformation where
  parseJSON =
    Lude.withObject
      "ErrorInformation"
      ( \x ->
          ErrorInformation'
            Lude.<$> (x Lude..:? "code") Lude.<*> (x Lude..:? "message")
      )
