{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Deployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.Deployment
  ( Deployment (..)
  -- * Smart constructor
  , mkDeployment
  -- * Lenses
  , dAppId
  , dCommand
  , dComment
  , dCompletedAt
  , dCreatedAt
  , dCustomJson
  , dDeploymentId
  , dDuration
  , dIamUserArn
  , dInstanceIds
  , dStackId
  , dStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.DateTime as Types
import qualified Network.AWS.OpsWorks.Types.DeploymentCommand as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a deployment of a stack or app.
--
-- /See:/ 'mkDeployment' smart constructor.
data Deployment = Deployment'
  { appId :: Core.Maybe Core.Text
    -- ^ The app ID.
  , command :: Core.Maybe Types.DeploymentCommand
    -- ^ Used to specify a stack or deployment command.
  , comment :: Core.Maybe Core.Text
    -- ^ A user-defined comment.
  , completedAt :: Core.Maybe Types.DateTime
    -- ^ Date when the deployment completed.
  , createdAt :: Core.Maybe Types.DateTime
    -- ^ Date when the deployment was created.
  , customJson :: Core.Maybe Core.Text
    -- ^ A string that contains user-defined custom JSON. It can be used to override the corresponding default stack configuration attribute values for stack or to pass data to recipes. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@ 
-- For more information on custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
  , deploymentId :: Core.Maybe Core.Text
    -- ^ The deployment ID.
  , duration :: Core.Maybe Core.Int
    -- ^ The deployment duration.
  , iamUserArn :: Core.Maybe Core.Text
    -- ^ The user's IAM ARN.
  , instanceIds :: Core.Maybe [Core.Text]
    -- ^ The IDs of the target instances.
  , stackId :: Core.Maybe Core.Text
    -- ^ The stack ID.
  , status :: Core.Maybe Core.Text
    -- ^ The deployment status:
--
--
--     * running
--
--
--     * successful
--
--
--     * failed
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Deployment' value with any optional fields omitted.
mkDeployment
    :: Deployment
mkDeployment
  = Deployment'{appId = Core.Nothing, command = Core.Nothing,
                comment = Core.Nothing, completedAt = Core.Nothing,
                createdAt = Core.Nothing, customJson = Core.Nothing,
                deploymentId = Core.Nothing, duration = Core.Nothing,
                iamUserArn = Core.Nothing, instanceIds = Core.Nothing,
                stackId = Core.Nothing, status = Core.Nothing}

-- | The app ID.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAppId :: Lens.Lens' Deployment (Core.Maybe Core.Text)
dAppId = Lens.field @"appId"
{-# INLINEABLE dAppId #-}
{-# DEPRECATED appId "Use generic-lens or generic-optics with 'appId' instead"  #-}

-- | Used to specify a stack or deployment command.
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCommand :: Lens.Lens' Deployment (Core.Maybe Types.DeploymentCommand)
dCommand = Lens.field @"command"
{-# INLINEABLE dCommand #-}
{-# DEPRECATED command "Use generic-lens or generic-optics with 'command' instead"  #-}

-- | A user-defined comment.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dComment :: Lens.Lens' Deployment (Core.Maybe Core.Text)
dComment = Lens.field @"comment"
{-# INLINEABLE dComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

-- | Date when the deployment completed.
--
-- /Note:/ Consider using 'completedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCompletedAt :: Lens.Lens' Deployment (Core.Maybe Types.DateTime)
dCompletedAt = Lens.field @"completedAt"
{-# INLINEABLE dCompletedAt #-}
{-# DEPRECATED completedAt "Use generic-lens or generic-optics with 'completedAt' instead"  #-}

-- | Date when the deployment was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreatedAt :: Lens.Lens' Deployment (Core.Maybe Types.DateTime)
dCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE dCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | A string that contains user-defined custom JSON. It can be used to override the corresponding default stack configuration attribute values for stack or to pass data to recipes. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@ 
-- For more information on custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
--
-- /Note:/ Consider using 'customJson' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCustomJson :: Lens.Lens' Deployment (Core.Maybe Core.Text)
dCustomJson = Lens.field @"customJson"
{-# INLINEABLE dCustomJson #-}
{-# DEPRECATED customJson "Use generic-lens or generic-optics with 'customJson' instead"  #-}

-- | The deployment ID.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeploymentId :: Lens.Lens' Deployment (Core.Maybe Core.Text)
dDeploymentId = Lens.field @"deploymentId"
{-# INLINEABLE dDeploymentId #-}
{-# DEPRECATED deploymentId "Use generic-lens or generic-optics with 'deploymentId' instead"  #-}

-- | The deployment duration.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDuration :: Lens.Lens' Deployment (Core.Maybe Core.Int)
dDuration = Lens.field @"duration"
{-# INLINEABLE dDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | The user's IAM ARN.
--
-- /Note:/ Consider using 'iamUserArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dIamUserArn :: Lens.Lens' Deployment (Core.Maybe Core.Text)
dIamUserArn = Lens.field @"iamUserArn"
{-# INLINEABLE dIamUserArn #-}
{-# DEPRECATED iamUserArn "Use generic-lens or generic-optics with 'iamUserArn' instead"  #-}

-- | The IDs of the target instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInstanceIds :: Lens.Lens' Deployment (Core.Maybe [Core.Text])
dInstanceIds = Lens.field @"instanceIds"
{-# INLINEABLE dInstanceIds #-}
{-# DEPRECATED instanceIds "Use generic-lens or generic-optics with 'instanceIds' instead"  #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStackId :: Lens.Lens' Deployment (Core.Maybe Core.Text)
dStackId = Lens.field @"stackId"
{-# INLINEABLE dStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

-- | The deployment status:
--
--
--     * running
--
--
--     * successful
--
--
--     * failed
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStatus :: Lens.Lens' Deployment (Core.Maybe Core.Text)
dStatus = Lens.field @"status"
{-# INLINEABLE dStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON Deployment where
        parseJSON
          = Core.withObject "Deployment" Core.$
              \ x ->
                Deployment' Core.<$>
                  (x Core..:? "AppId") Core.<*> x Core..:? "Command" Core.<*>
                    x Core..:? "Comment"
                    Core.<*> x Core..:? "CompletedAt"
                    Core.<*> x Core..:? "CreatedAt"
                    Core.<*> x Core..:? "CustomJson"
                    Core.<*> x Core..:? "DeploymentId"
                    Core.<*> x Core..:? "Duration"
                    Core.<*> x Core..:? "IamUserArn"
                    Core.<*> x Core..:? "InstanceIds"
                    Core.<*> x Core..:? "StackId"
                    Core.<*> x Core..:? "Status"
