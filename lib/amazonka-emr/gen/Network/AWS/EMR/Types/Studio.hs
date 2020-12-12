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
    stuCreationTime,
    stuEngineSecurityGroupId,
    stuSubnetIds,
    stuStudioId,
    stuVPCId,
    stuURL,
    stuAuthMode,
    stuDefaultS3Location,
    stuWorkspaceSecurityGroupId,
    stuName,
    stuStudioARN,
    stuUserRole,
    stuDescription,
    stuTags,
    stuServiceRole,
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
  { creationTime :: Lude.Maybe Lude.Timestamp,
    engineSecurityGroupId :: Lude.Maybe Lude.Text,
    subnetIds :: Lude.Maybe [Lude.Text],
    studioId :: Lude.Maybe Lude.Text,
    vpcId :: Lude.Maybe Lude.Text,
    url :: Lude.Maybe Lude.Text,
    authMode :: Lude.Maybe AuthMode,
    defaultS3Location :: Lude.Maybe Lude.Text,
    workspaceSecurityGroupId :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    studioARN :: Lude.Maybe Lude.Text,
    userRole :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    serviceRole :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Studio' with the minimum fields required to make a request.
--
-- * 'authMode' - Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM.
-- * 'creationTime' - The time the Amazon EMR Studio was created.
-- * 'defaultS3Location' - The default Amazon S3 location to back up Amazon EMR Studio Workspaces and notebook files.
-- * 'description' - The detailed description of the EMR Studio.
-- * 'engineSecurityGroupId' - The ID of the Engine security group associated with the Amazon EMR Studio. The Engine security group allows inbound network traffic from resources in the Workspace security group.
-- * 'name' - The name of the EMR Studio.
-- * 'serviceRole' - The name of the IAM role assumed by the Amazon EMR Studio.
-- * 'studioARN' - The Amazon Resource Name (ARN) of the EMR Studio.
-- * 'studioId' - The ID of the EMR Studio.
-- * 'subnetIds' - The list of IDs of the subnets associated with the Amazon EMR Studio.
-- * 'tags' - A list of tags associated with the Amazon EMR Studio.
-- * 'url' - The unique access URL of the Amazon EMR Studio.
-- * 'userRole' - The name of the IAM role assumed by users logged in to the Amazon EMR Studio.
-- * 'vpcId' - The ID of the VPC associated with the EMR Studio.
-- * 'workspaceSecurityGroupId' - The ID of the Workspace security group associated with the Amazon EMR Studio. The Workspace security group allows outbound network traffic to resources in the Engine security group and to the internet.
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
stuCreationTime :: Lens.Lens' Studio (Lude.Maybe Lude.Timestamp)
stuCreationTime = Lens.lens (creationTime :: Studio -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: Studio)
{-# DEPRECATED stuCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The ID of the Engine security group associated with the Amazon EMR Studio. The Engine security group allows inbound network traffic from resources in the Workspace security group.
--
-- /Note:/ Consider using 'engineSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stuEngineSecurityGroupId :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
stuEngineSecurityGroupId = Lens.lens (engineSecurityGroupId :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {engineSecurityGroupId = a} :: Studio)
{-# DEPRECATED stuEngineSecurityGroupId "Use generic-lens or generic-optics with 'engineSecurityGroupId' instead." #-}

-- | The list of IDs of the subnets associated with the Amazon EMR Studio.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stuSubnetIds :: Lens.Lens' Studio (Lude.Maybe [Lude.Text])
stuSubnetIds = Lens.lens (subnetIds :: Studio -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: Studio)
{-# DEPRECATED stuSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The ID of the EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stuStudioId :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
stuStudioId = Lens.lens (studioId :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {studioId = a} :: Studio)
{-# DEPRECATED stuStudioId "Use generic-lens or generic-optics with 'studioId' instead." #-}

-- | The ID of the VPC associated with the EMR Studio.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stuVPCId :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
stuVPCId = Lens.lens (vpcId :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: Studio)
{-# DEPRECATED stuVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The unique access URL of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stuURL :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
stuURL = Lens.lens (url :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: Studio)
{-# DEPRECATED stuURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM.
--
-- /Note:/ Consider using 'authMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stuAuthMode :: Lens.Lens' Studio (Lude.Maybe AuthMode)
stuAuthMode = Lens.lens (authMode :: Studio -> Lude.Maybe AuthMode) (\s a -> s {authMode = a} :: Studio)
{-# DEPRECATED stuAuthMode "Use generic-lens or generic-optics with 'authMode' instead." #-}

-- | The default Amazon S3 location to back up Amazon EMR Studio Workspaces and notebook files.
--
-- /Note:/ Consider using 'defaultS3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stuDefaultS3Location :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
stuDefaultS3Location = Lens.lens (defaultS3Location :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {defaultS3Location = a} :: Studio)
{-# DEPRECATED stuDefaultS3Location "Use generic-lens or generic-optics with 'defaultS3Location' instead." #-}

-- | The ID of the Workspace security group associated with the Amazon EMR Studio. The Workspace security group allows outbound network traffic to resources in the Engine security group and to the internet.
--
-- /Note:/ Consider using 'workspaceSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stuWorkspaceSecurityGroupId :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
stuWorkspaceSecurityGroupId = Lens.lens (workspaceSecurityGroupId :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {workspaceSecurityGroupId = a} :: Studio)
{-# DEPRECATED stuWorkspaceSecurityGroupId "Use generic-lens or generic-optics with 'workspaceSecurityGroupId' instead." #-}

-- | The name of the EMR Studio.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stuName :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
stuName = Lens.lens (name :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Studio)
{-# DEPRECATED stuName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon Resource Name (ARN) of the EMR Studio.
--
-- /Note:/ Consider using 'studioARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stuStudioARN :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
stuStudioARN = Lens.lens (studioARN :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {studioARN = a} :: Studio)
{-# DEPRECATED stuStudioARN "Use generic-lens or generic-optics with 'studioARN' instead." #-}

-- | The name of the IAM role assumed by users logged in to the Amazon EMR Studio.
--
-- /Note:/ Consider using 'userRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stuUserRole :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
stuUserRole = Lens.lens (userRole :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {userRole = a} :: Studio)
{-# DEPRECATED stuUserRole "Use generic-lens or generic-optics with 'userRole' instead." #-}

-- | The detailed description of the EMR Studio.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stuDescription :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
stuDescription = Lens.lens (description :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Studio)
{-# DEPRECATED stuDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tags associated with the Amazon EMR Studio.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stuTags :: Lens.Lens' Studio (Lude.Maybe [Tag])
stuTags = Lens.lens (tags :: Studio -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Studio)
{-# DEPRECATED stuTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the IAM role assumed by the Amazon EMR Studio.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stuServiceRole :: Lens.Lens' Studio (Lude.Maybe Lude.Text)
stuServiceRole = Lens.lens (serviceRole :: Studio -> Lude.Maybe Lude.Text) (\s a -> s {serviceRole = a} :: Studio)
{-# DEPRECATED stuServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

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
