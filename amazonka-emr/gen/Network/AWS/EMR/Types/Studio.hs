{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Studio
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Studio where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.AuthMode
import Network.AWS.EMR.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Details for an Amazon EMR Studio including ID, creation time, name, and
-- so on.
--
-- /See:/ 'newStudio' smart constructor.
data Studio = Studio'
  { -- | The time the Amazon EMR Studio was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The name of the IAM role assumed by the Amazon EMR Studio.
    serviceRole :: Core.Maybe Core.Text,
    -- | The ID of the Workspace security group associated with the Amazon EMR
    -- Studio. The Workspace security group allows outbound network traffic to
    -- resources in the Engine security group and to the internet.
    workspaceSecurityGroupId :: Core.Maybe Core.Text,
    -- | The default Amazon S3 location to back up Amazon EMR Studio Workspaces
    -- and notebook files.
    defaultS3Location :: Core.Maybe Core.Text,
    -- | Specifies whether the Amazon EMR Studio authenticates users using single
    -- sign-on (SSO) or IAM.
    authMode :: Core.Maybe AuthMode,
    -- | The list of IDs of the subnets associated with the Amazon EMR Studio.
    subnetIds :: Core.Maybe [Core.Text],
    -- | The name of the IAM role assumed by users logged in to the Amazon EMR
    -- Studio.
    userRole :: Core.Maybe Core.Text,
    -- | The name of the Amazon EMR Studio.
    name :: Core.Maybe Core.Text,
    -- | A list of tags associated with the Amazon EMR Studio.
    tags :: Core.Maybe [Tag],
    -- | The detailed description of the Amazon EMR Studio.
    description :: Core.Maybe Core.Text,
    -- | The unique access URL of the Amazon EMR Studio.
    url :: Core.Maybe Core.Text,
    -- | The ID of the VPC associated with the Amazon EMR Studio.
    vpcId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon EMR Studio.
    studioArn :: Core.Maybe Core.Text,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Core.Maybe Core.Text,
    -- | The ID of the Engine security group associated with the Amazon EMR
    -- Studio. The Engine security group allows inbound network traffic from
    -- resources in the Workspace security group.
    engineSecurityGroupId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Studio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'studio_creationTime' - The time the Amazon EMR Studio was created.
--
-- 'serviceRole', 'studio_serviceRole' - The name of the IAM role assumed by the Amazon EMR Studio.
--
-- 'workspaceSecurityGroupId', 'studio_workspaceSecurityGroupId' - The ID of the Workspace security group associated with the Amazon EMR
-- Studio. The Workspace security group allows outbound network traffic to
-- resources in the Engine security group and to the internet.
--
-- 'defaultS3Location', 'studio_defaultS3Location' - The default Amazon S3 location to back up Amazon EMR Studio Workspaces
-- and notebook files.
--
-- 'authMode', 'studio_authMode' - Specifies whether the Amazon EMR Studio authenticates users using single
-- sign-on (SSO) or IAM.
--
-- 'subnetIds', 'studio_subnetIds' - The list of IDs of the subnets associated with the Amazon EMR Studio.
--
-- 'userRole', 'studio_userRole' - The name of the IAM role assumed by users logged in to the Amazon EMR
-- Studio.
--
-- 'name', 'studio_name' - The name of the Amazon EMR Studio.
--
-- 'tags', 'studio_tags' - A list of tags associated with the Amazon EMR Studio.
--
-- 'description', 'studio_description' - The detailed description of the Amazon EMR Studio.
--
-- 'url', 'studio_url' - The unique access URL of the Amazon EMR Studio.
--
-- 'vpcId', 'studio_vpcId' - The ID of the VPC associated with the Amazon EMR Studio.
--
-- 'studioArn', 'studio_studioArn' - The Amazon Resource Name (ARN) of the Amazon EMR Studio.
--
-- 'studioId', 'studio_studioId' - The ID of the Amazon EMR Studio.
--
-- 'engineSecurityGroupId', 'studio_engineSecurityGroupId' - The ID of the Engine security group associated with the Amazon EMR
-- Studio. The Engine security group allows inbound network traffic from
-- resources in the Workspace security group.
newStudio ::
  Studio
newStudio =
  Studio'
    { creationTime = Core.Nothing,
      serviceRole = Core.Nothing,
      workspaceSecurityGroupId = Core.Nothing,
      defaultS3Location = Core.Nothing,
      authMode = Core.Nothing,
      subnetIds = Core.Nothing,
      userRole = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      url = Core.Nothing,
      vpcId = Core.Nothing,
      studioArn = Core.Nothing,
      studioId = Core.Nothing,
      engineSecurityGroupId = Core.Nothing
    }

-- | The time the Amazon EMR Studio was created.
studio_creationTime :: Lens.Lens' Studio (Core.Maybe Core.UTCTime)
studio_creationTime = Lens.lens (\Studio' {creationTime} -> creationTime) (\s@Studio' {} a -> s {creationTime = a} :: Studio) Core.. Lens.mapping Core._Time

-- | The name of the IAM role assumed by the Amazon EMR Studio.
studio_serviceRole :: Lens.Lens' Studio (Core.Maybe Core.Text)
studio_serviceRole = Lens.lens (\Studio' {serviceRole} -> serviceRole) (\s@Studio' {} a -> s {serviceRole = a} :: Studio)

-- | The ID of the Workspace security group associated with the Amazon EMR
-- Studio. The Workspace security group allows outbound network traffic to
-- resources in the Engine security group and to the internet.
studio_workspaceSecurityGroupId :: Lens.Lens' Studio (Core.Maybe Core.Text)
studio_workspaceSecurityGroupId = Lens.lens (\Studio' {workspaceSecurityGroupId} -> workspaceSecurityGroupId) (\s@Studio' {} a -> s {workspaceSecurityGroupId = a} :: Studio)

-- | The default Amazon S3 location to back up Amazon EMR Studio Workspaces
-- and notebook files.
studio_defaultS3Location :: Lens.Lens' Studio (Core.Maybe Core.Text)
studio_defaultS3Location = Lens.lens (\Studio' {defaultS3Location} -> defaultS3Location) (\s@Studio' {} a -> s {defaultS3Location = a} :: Studio)

-- | Specifies whether the Amazon EMR Studio authenticates users using single
-- sign-on (SSO) or IAM.
studio_authMode :: Lens.Lens' Studio (Core.Maybe AuthMode)
studio_authMode = Lens.lens (\Studio' {authMode} -> authMode) (\s@Studio' {} a -> s {authMode = a} :: Studio)

-- | The list of IDs of the subnets associated with the Amazon EMR Studio.
studio_subnetIds :: Lens.Lens' Studio (Core.Maybe [Core.Text])
studio_subnetIds = Lens.lens (\Studio' {subnetIds} -> subnetIds) (\s@Studio' {} a -> s {subnetIds = a} :: Studio) Core.. Lens.mapping Lens._Coerce

-- | The name of the IAM role assumed by users logged in to the Amazon EMR
-- Studio.
studio_userRole :: Lens.Lens' Studio (Core.Maybe Core.Text)
studio_userRole = Lens.lens (\Studio' {userRole} -> userRole) (\s@Studio' {} a -> s {userRole = a} :: Studio)

-- | The name of the Amazon EMR Studio.
studio_name :: Lens.Lens' Studio (Core.Maybe Core.Text)
studio_name = Lens.lens (\Studio' {name} -> name) (\s@Studio' {} a -> s {name = a} :: Studio)

-- | A list of tags associated with the Amazon EMR Studio.
studio_tags :: Lens.Lens' Studio (Core.Maybe [Tag])
studio_tags = Lens.lens (\Studio' {tags} -> tags) (\s@Studio' {} a -> s {tags = a} :: Studio) Core.. Lens.mapping Lens._Coerce

-- | The detailed description of the Amazon EMR Studio.
studio_description :: Lens.Lens' Studio (Core.Maybe Core.Text)
studio_description = Lens.lens (\Studio' {description} -> description) (\s@Studio' {} a -> s {description = a} :: Studio)

-- | The unique access URL of the Amazon EMR Studio.
studio_url :: Lens.Lens' Studio (Core.Maybe Core.Text)
studio_url = Lens.lens (\Studio' {url} -> url) (\s@Studio' {} a -> s {url = a} :: Studio)

-- | The ID of the VPC associated with the Amazon EMR Studio.
studio_vpcId :: Lens.Lens' Studio (Core.Maybe Core.Text)
studio_vpcId = Lens.lens (\Studio' {vpcId} -> vpcId) (\s@Studio' {} a -> s {vpcId = a} :: Studio)

-- | The Amazon Resource Name (ARN) of the Amazon EMR Studio.
studio_studioArn :: Lens.Lens' Studio (Core.Maybe Core.Text)
studio_studioArn = Lens.lens (\Studio' {studioArn} -> studioArn) (\s@Studio' {} a -> s {studioArn = a} :: Studio)

-- | The ID of the Amazon EMR Studio.
studio_studioId :: Lens.Lens' Studio (Core.Maybe Core.Text)
studio_studioId = Lens.lens (\Studio' {studioId} -> studioId) (\s@Studio' {} a -> s {studioId = a} :: Studio)

-- | The ID of the Engine security group associated with the Amazon EMR
-- Studio. The Engine security group allows inbound network traffic from
-- resources in the Workspace security group.
studio_engineSecurityGroupId :: Lens.Lens' Studio (Core.Maybe Core.Text)
studio_engineSecurityGroupId = Lens.lens (\Studio' {engineSecurityGroupId} -> engineSecurityGroupId) (\s@Studio' {} a -> s {engineSecurityGroupId = a} :: Studio)

instance Core.FromJSON Studio where
  parseJSON =
    Core.withObject
      "Studio"
      ( \x ->
          Studio'
            Core.<$> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "ServiceRole")
            Core.<*> (x Core..:? "WorkspaceSecurityGroupId")
            Core.<*> (x Core..:? "DefaultS3Location")
            Core.<*> (x Core..:? "AuthMode")
            Core.<*> (x Core..:? "SubnetIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "UserRole")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "Url")
            Core.<*> (x Core..:? "VpcId")
            Core.<*> (x Core..:? "StudioArn")
            Core.<*> (x Core..:? "StudioId")
            Core.<*> (x Core..:? "EngineSecurityGroupId")
      )

instance Core.Hashable Studio

instance Core.NFData Studio
