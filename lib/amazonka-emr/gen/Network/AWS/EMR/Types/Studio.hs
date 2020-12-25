{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Studio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Studio
  ( Studio (..),

    -- * Smart constructor
    mkStudio,

    -- * Lenses
    sgAuthMode,
    sgCreationTime,
    sgDefaultS3Location,
    sgDescription,
    sgEngineSecurityGroupId,
    sgName,
    sgServiceRole,
    sgStudioArn,
    sgStudioId,
    sgSubnetIds,
    sgTags,
    sgUrl,
    sgUserRole,
    sgVpcId,
    sgWorkspaceSecurityGroupId,
  )
where

import qualified Network.AWS.EMR.Types.AuthMode as Types
import qualified Network.AWS.EMR.Types.String as Types
import qualified Network.AWS.EMR.Types.Tag as Types
import qualified Network.AWS.EMR.Types.XmlString as Types
import qualified Network.AWS.EMR.Types.XmlStringMaxLen256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details for an Amazon EMR Studio including ID, creation time, name, and so on.
--
-- /See:/ 'mkStudio' smart constructor.
data Studio = Studio'
  { -- | Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM.
    authMode :: Core.Maybe Types.AuthMode,
    -- | The time the Amazon EMR Studio was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The default Amazon S3 location to back up Amazon EMR Studio Workspaces and notebook files.
    defaultS3Location :: Core.Maybe Types.XmlString,
    -- | The detailed description of the EMR Studio.
    description :: Core.Maybe Types.XmlStringMaxLen256,
    -- | The ID of the Engine security group associated with the Amazon EMR Studio. The Engine security group allows inbound network traffic from resources in the Workspace security group.
    engineSecurityGroupId :: Core.Maybe Types.XmlStringMaxLen256,
    -- | The name of the EMR Studio.
    name :: Core.Maybe Types.XmlStringMaxLen256,
    -- | The name of the IAM role assumed by the Amazon EMR Studio.
    serviceRole :: Core.Maybe Types.XmlString,
    -- | The Amazon Resource Name (ARN) of the EMR Studio.
    studioArn :: Core.Maybe Types.XmlStringMaxLen256,
    -- | The ID of the EMR Studio.
    studioId :: Core.Maybe Types.XmlStringMaxLen256,
    -- | The list of IDs of the subnets associated with the Amazon EMR Studio.
    subnetIds :: Core.Maybe [Types.String],
    -- | A list of tags associated with the Amazon EMR Studio.
    tags :: Core.Maybe [Types.Tag],
    -- | The unique access URL of the Amazon EMR Studio.
    url :: Core.Maybe Types.XmlString,
    -- | The name of the IAM role assumed by users logged in to the Amazon EMR Studio.
    userRole :: Core.Maybe Types.XmlString,
    -- | The ID of the VPC associated with the EMR Studio.
    vpcId :: Core.Maybe Types.XmlStringMaxLen256,
    -- | The ID of the Workspace security group associated with the Amazon EMR Studio. The Workspace security group allows outbound network traffic to resources in the Engine security group and to the internet.
    workspaceSecurityGroupId :: Core.Maybe Types.XmlStringMaxLen256
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Studio' value with any optional fields omitted.
mkStudio ::
  Studio
mkStudio =
  Studio'
    { authMode = Core.Nothing,
      creationTime = Core.Nothing,
      defaultS3Location = Core.Nothing,
      description = Core.Nothing,
      engineSecurityGroupId = Core.Nothing,
      name = Core.Nothing,
      serviceRole = Core.Nothing,
      studioArn = Core.Nothing,
      studioId = Core.Nothing,
      subnetIds = Core.Nothing,
      tags = Core.Nothing,
      url = Core.Nothing,
      userRole = Core.Nothing,
      vpcId = Core.Nothing,
      workspaceSecurityGroupId = Core.Nothing
    }

-- | Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM.
--
-- /Note:/ Consider using 'authMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgAuthMode :: Lens.Lens' Studio (Core.Maybe Types.AuthMode)
sgAuthMode = Lens.field @"authMode"
{-# DEPRECATED sgAuthMode "Use generic-lens or generic-optics with 'authMode' instead." #-}

-- | The time the Amazon EMR Studio was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgCreationTime :: Lens.Lens' Studio (Core.Maybe Core.NominalDiffTime)
sgCreationTime = Lens.field @"creationTime"
{-# DEPRECATED sgCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The default Amazon S3 location to back up Amazon EMR Studio Workspaces and notebook files.
--
-- /Note:/ Consider using 'defaultS3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgDefaultS3Location :: Lens.Lens' Studio (Core.Maybe Types.XmlString)
sgDefaultS3Location = Lens.field @"defaultS3Location"
{-# DEPRECATED sgDefaultS3Location "Use generic-lens or generic-optics with 'defaultS3Location' instead." #-}

-- | The detailed description of the EMR Studio.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgDescription :: Lens.Lens' Studio (Core.Maybe Types.XmlStringMaxLen256)
sgDescription = Lens.field @"description"
{-# DEPRECATED sgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the Engine security group associated with the Amazon EMR Studio. The Engine security group allows inbound network traffic from resources in the Workspace security group.
--
-- /Note:/ Consider using 'engineSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgEngineSecurityGroupId :: Lens.Lens' Studio (Core.Maybe Types.XmlStringMaxLen256)
sgEngineSecurityGroupId = Lens.field @"engineSecurityGroupId"
{-# DEPRECATED sgEngineSecurityGroupId "Use generic-lens or generic-optics with 'engineSecurityGroupId' instead." #-}

-- | The name of the EMR Studio.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgName :: Lens.Lens' Studio (Core.Maybe Types.XmlStringMaxLen256)
sgName = Lens.field @"name"
{-# DEPRECATED sgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the IAM role assumed by the Amazon EMR Studio.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgServiceRole :: Lens.Lens' Studio (Core.Maybe Types.XmlString)
sgServiceRole = Lens.field @"serviceRole"
{-# DEPRECATED sgServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | The Amazon Resource Name (ARN) of the EMR Studio.
--
-- /Note:/ Consider using 'studioArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgStudioArn :: Lens.Lens' Studio (Core.Maybe Types.XmlStringMaxLen256)
sgStudioArn = Lens.field @"studioArn"
{-# DEPRECATED sgStudioArn "Use generic-lens or generic-optics with 'studioArn' instead." #-}

-- | The ID of the EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgStudioId :: Lens.Lens' Studio (Core.Maybe Types.XmlStringMaxLen256)
sgStudioId = Lens.field @"studioId"
{-# DEPRECATED sgStudioId "Use generic-lens or generic-optics with 'studioId' instead." #-}

-- | The list of IDs of the subnets associated with the Amazon EMR Studio.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgSubnetIds :: Lens.Lens' Studio (Core.Maybe [Types.String])
sgSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED sgSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | A list of tags associated with the Amazon EMR Studio.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgTags :: Lens.Lens' Studio (Core.Maybe [Types.Tag])
sgTags = Lens.field @"tags"
{-# DEPRECATED sgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The unique access URL of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgUrl :: Lens.Lens' Studio (Core.Maybe Types.XmlString)
sgUrl = Lens.field @"url"
{-# DEPRECATED sgUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The name of the IAM role assumed by users logged in to the Amazon EMR Studio.
--
-- /Note:/ Consider using 'userRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgUserRole :: Lens.Lens' Studio (Core.Maybe Types.XmlString)
sgUserRole = Lens.field @"userRole"
{-# DEPRECATED sgUserRole "Use generic-lens or generic-optics with 'userRole' instead." #-}

-- | The ID of the VPC associated with the EMR Studio.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgVpcId :: Lens.Lens' Studio (Core.Maybe Types.XmlStringMaxLen256)
sgVpcId = Lens.field @"vpcId"
{-# DEPRECATED sgVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The ID of the Workspace security group associated with the Amazon EMR Studio. The Workspace security group allows outbound network traffic to resources in the Engine security group and to the internet.
--
-- /Note:/ Consider using 'workspaceSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgWorkspaceSecurityGroupId :: Lens.Lens' Studio (Core.Maybe Types.XmlStringMaxLen256)
sgWorkspaceSecurityGroupId = Lens.field @"workspaceSecurityGroupId"
{-# DEPRECATED sgWorkspaceSecurityGroupId "Use generic-lens or generic-optics with 'workspaceSecurityGroupId' instead." #-}

instance Core.FromJSON Studio where
  parseJSON =
    Core.withObject "Studio" Core.$
      \x ->
        Studio'
          Core.<$> (x Core..:? "AuthMode")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "DefaultS3Location")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "EngineSecurityGroupId")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "ServiceRole")
          Core.<*> (x Core..:? "StudioArn")
          Core.<*> (x Core..:? "StudioId")
          Core.<*> (x Core..:? "SubnetIds")
          Core.<*> (x Core..:? "Tags")
          Core.<*> (x Core..:? "Url")
          Core.<*> (x Core..:? "UserRole")
          Core.<*> (x Core..:? "VpcId")
          Core.<*> (x Core..:? "WorkspaceSecurityGroupId")
