{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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

import qualified Network.AWS.CodeDeploy.Types.DeployErrorCode as Types
import qualified Network.AWS.CodeDeploy.Types.Message as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a deployment error.
--
-- /See:/ 'mkErrorInformation' smart constructor.
data ErrorInformation = ErrorInformation'
  { -- | For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/error-codes.html Error Codes for AWS CodeDeploy> in the <https://docs.aws.amazon.com/codedeploy/latest/userguide AWS CodeDeploy User Guide> .
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
    code :: Core.Maybe Types.DeployErrorCode,
    -- | An accompanying error message.
    message :: Core.Maybe Types.Message
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ErrorInformation' value with any optional fields omitted.
mkErrorInformation ::
  ErrorInformation
mkErrorInformation =
  ErrorInformation' {code = Core.Nothing, message = Core.Nothing}

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
eiCode :: Lens.Lens' ErrorInformation (Core.Maybe Types.DeployErrorCode)
eiCode = Lens.field @"code"
{-# DEPRECATED eiCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | An accompanying error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiMessage :: Lens.Lens' ErrorInformation (Core.Maybe Types.Message)
eiMessage = Lens.field @"message"
{-# DEPRECATED eiMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromJSON ErrorInformation where
  parseJSON =
    Core.withObject "ErrorInformation" Core.$
      \x ->
        ErrorInformation'
          Core.<$> (x Core..:? "code") Core.<*> (x Core..:? "message")
