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
    sfCreationTime,
    sfEngineSecurityGroupId,
    sfSubnetIds,
    sfStudioId,
    sfVPCId,
    sfURL,
    sfAuthMode,
    sfDefaultS3Location,
    sfWorkspaceSecurityGroupId,
    sfName,
    sfStudioARN,
    sfUserRole,
    sfDescription,
    sfTags,
    sfServiceRole,
  )
where

import Network.AWS.EMR.Types.AuthMode
import Network.AWS.EMR.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details for an Amazon EMR Studio including ID, creation time, name, and so on.
--
-- /See:/ 'mkStudio' smart constructor.
data Studio = Studio'
  { -- | The time the Amazon EMR Studio was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The ID of the Engine security group associated with the Amazon EMR Studio. The Engine security group allows inbound network traffic from resources in the Workspace security group.
    engineSecurityGroupId :: Lude.Maybe Lude.Text,
    -- | The list of IDs of the subnets associated with the Amazon EMR Studio.
    subnetIds :: Lude.Maybe [Lude.Text],
    -- | The ID of the EMR Studio.
    studioId :: Lude.Maybe Lude.Text,
    -- | The ID of the VPC associated with the EMR Studio.
    vpcId :: Lude.Maybe Lude.Text,
    -- | The unique access URL of the Amazon EMR Studio.
    url :: Lude.Maybe Lude.Text,
    -- | Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM.
    authMode :: Lude.Maybe AuthMode,
    -- | The default Amazon S3 location to back up Amazon EMR Studio Workspaces and notebook files.
    defaultS3Location :: Lude.Maybe Lude.Text,
    -- | The ID of the Workspace security group associated with the Amazon EMR Studio. The Workspace security group allows outbound network traffic to resources in the Engine security group and to the internet.
    workspaceSecurityGroupId :: Lude.Maybe Lude.Text,
    -- | The name of the EMR Studio.
    name :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the EMR Studio.
    studioARN :: Lude.Maybe Lude.Text,
    -- | The name of the IAM role assumed by users logged in to the Amazon EMR Studio.
    userRole :: Lude.Maybe Lude.Text,
    -- | The detailed description of the EMR Studio.
    description :: Lude.Maybe Lude.Text,
    -- | A list of tags associated with the Amazon EMR Studio.
    tags :: Lude.Maybe [Tag],
    -- | The name of the IAM role assumed by the Amazon EMR Studio.
    serviceRole :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Studio' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time the Amazon EMR Studio was created.
-- * 'engineSecurityGroupId' - The ID of the Engine security group associated with the Amazon EMR Studio. The Engine security group allows inbound network traffic from resources in the Workspace security group.
-- * 'subnetIds' - The list of IDs of the subnets associated with the Amazon EMR Studio.
-- * 'studioId' - The ID of the EMR Studio.
-- * 'vpcId' - The ID of the VPC associated with the EMR Studio.
-- * 'url' - The unique access URL of the Amazon EMR Studio.
-- * 'authMode' - Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM.
-- * 'defaultS3Location' - The default Amazon S3 location to back up Amazon EMR Studio Workspaces and notebook files.
-- * 'workspaceSecurityGroupId' - The ID of the Workspace security group associated with the Amazon EMR Studio. The Workspace security group allows outbound network traffic to resources in the Engine security group and to the internet.
-- * 'name' - The name of the EMR Studio.
-- * 'studioARN' - The Amazon Resource Name (ARN) of the EMR Studio.
-- * 'userRole' - The name of the IAM role assumed by users logged in to the Amazon EMR Studio.
-- * 'description' - The detailed description of the EMR Studio.
-- * 'tags' - A list of tags associated with the Amazon EMR Studio.
-- * 'serviceRole' - The name of the IAM role assumed by the Amazon EMR Studio.
mkStudio ::
  Studio
mkStudio =
  Studio'
    { creationTime = Lude.Nothing,
      engineSecurityGroupId = Lude.Nothing,
      subnetIds = Lude.Nothing,
      studioId = Lude.Nothing,
      vpcId = Lude.Nothing,
      url = Lude.Nothing,
      authMode = Lude.Nothing,
      defaultS3Location = Lude.Nothing,
      workspaceSecurityGroupId = Lude.Nothing,
      name = Lude.Nothing,
      studioARN = Lude.Nothing,
      userRole = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      serviceRole = Lude.Nothing
    }

-- | The time the Amazon EMR Studio was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfCreationTime :: Lens.Lens' Studio (Lude.Maybe Lude.Timestamp)
sfCreationTime = Lens.lens (creationTime :: Studio -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: Studio)
{-# DEPRECATED sfCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The ID of the Engine security group associated with the Amazon EMR Studio. The Engine security group allows inbound network traffic from resources in the Workspace security group.
--
-- /Note:/ Consider using 'engineSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfEngineSecurityGroupId :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
sfEngineSecurityGroupId = Lens.lens (engineSecurityGroupId :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {engineSecurityGroupId = a} :: Studio)
{-# DEPRECATED sfEngineSecurityGroupId "Use generic-lens or generic-optics with 'engineSecurityGroupId' instead." #-}

-- | The list of IDs of the subnets associated with the Amazon EMR Studio.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfSubnetIds :: Lens.Lens' Studio (Lude.Maybe [Lude.Text])
sfSubnetIds = Lens.lens (subnetIds :: Studio -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: Studio)
{-# DEPRECATED sfSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The ID of the EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfStudioId :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
sfStudioId = Lens.lens (studioId :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {studioId = a} :: Studio)
{-# DEPRECATED sfStudioId "Use generic-lens or generic-optics with 'studioId' instead." #-}

-- | The ID of the VPC associated with the EMR Studio.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfVPCId :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
sfVPCId = Lens.lens (vpcId :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: Studio)
{-# DEPRECATED sfVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The unique access URL of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfURL :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
sfURL = Lens.lens (url :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: Studio)
{-# DEPRECATED sfURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM.
--
-- /Note:/ Consider using 'authMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfAuthMode :: Lens.Lens' Studio (Lude.Maybe AuthMode)
sfAuthMode = Lens.lens (authMode :: Studio -> Lude.Maybe AuthMode) (\s a -> s {authMode = a} :: Studio)
{-# DEPRECATED sfAuthMode "Use generic-lens or generic-optics with 'authMode' instead." #-}

-- | The default Amazon S3 location to back up Amazon EMR Studio Workspaces and notebook files.
--
-- /Note:/ Consider using 'defaultS3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDefaultS3Location :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
sfDefaultS3Location = Lens.lens (defaultS3Location :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {defaultS3Location = a} :: Studio)
{-# DEPRECATED sfDefaultS3Location "Use generic-lens or generic-optics with 'defaultS3Location' instead." #-}

-- | The ID of the Workspace security group associated with the Amazon EMR Studio. The Workspace security group allows outbound network traffic to resources in the Engine security group and to the internet.
--
-- /Note:/ Consider using 'workspaceSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfWorkspaceSecurityGroupId :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
sfWorkspaceSecurityGroupId = Lens.lens (workspaceSecurityGroupId :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {workspaceSecurityGroupId = a} :: Studio)
{-# DEPRECATED sfWorkspaceSecurityGroupId "Use generic-lens or generic-optics with 'workspaceSecurityGroupId' instead." #-}

-- | The name of the EMR Studio.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfName :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
sfName = Lens.lens (name :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Studio)
{-# DEPRECATED sfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon Resource Name (ARN) of the EMR Studio.
--
-- /Note:/ Consider using 'studioARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfStudioARN :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
sfStudioARN = Lens.lens (studioARN :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {studioARN = a} :: Studio)
{-# DEPRECATED sfStudioARN "Use generic-lens or generic-optics with 'studioARN' instead." #-}

-- | The name of the IAM role assumed by users logged in to the Amazon EMR Studio.
--
-- /Note:/ Consider using 'userRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfUserRole :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
sfUserRole = Lens.lens (userRole :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {userRole = a} :: Studio)
{-# DEPRECATED sfUserRole "Use generic-lens or generic-optics with 'userRole' instead." #-}

-- | The detailed description of the EMR Studio.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDescription :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
sfDescription = Lens.lens (description :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Studio)
{-# DEPRECATED sfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tags associated with the Amazon EMR Studio.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfTags :: Lens.Lens' Studio (Lude.Maybe [Tag])
sfTags = Lens.lens (tags :: Studio -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Studio)
{-# DEPRECATED sfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the IAM role assumed by the Amazon EMR Studio.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfServiceRole :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
sfServiceRole = Lens.lens (serviceRole :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {serviceRole = a} :: Studio)
{-# DEPRECATED sfServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

instance Lude.FromJSON Studio where
  parseJSON =
    Lude.withObject
      "Studio"
      ( \x ->
          Studio'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "EngineSecurityGroupId")
            Lude.<*> (x Lude..:? "SubnetIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "StudioId")
            Lude.<*> (x Lude..:? "VpcId")
            Lude.<*> (x Lude..:? "Url")
            Lude.<*> (x Lude..:? "AuthMode")
            Lude.<*> (x Lude..:? "DefaultS3Location")
            Lude.<*> (x Lude..:? "WorkspaceSecurityGroupId")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "StudioArn")
            Lude.<*> (x Lude..:? "UserRole")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ServiceRole")
      )
