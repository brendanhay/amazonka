{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EMR.Types.AuthMode
import Network.AWS.EMR.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details for an Amazon EMR Studio including ID, creation time, name, and
-- so on.
--
-- /See:/ 'newStudio' smart constructor.
data Studio = Studio'
  { -- | The time the Amazon EMR Studio was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the IAM role assumed by the Amazon EMR Studio.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Workspace security group associated with the Amazon EMR
    -- Studio. The Workspace security group allows outbound network traffic to
    -- resources in the Engine security group and to the internet.
    workspaceSecurityGroupId :: Prelude.Maybe Prelude.Text,
    -- | The default Amazon S3 location to back up Amazon EMR Studio Workspaces
    -- and notebook files.
    defaultS3Location :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the Amazon EMR Studio authenticates users using single
    -- sign-on (SSO) or IAM.
    authMode :: Prelude.Maybe AuthMode,
    -- | The list of IDs of the subnets associated with the Amazon EMR Studio.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the IAM role assumed by users logged in to the Amazon EMR
    -- Studio.
    userRole :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon EMR Studio.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of tags associated with the Amazon EMR Studio.
    tags :: Prelude.Maybe [Tag],
    -- | The detailed description of the Amazon EMR Studio.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique access URL of the Amazon EMR Studio.
    url :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC associated with the Amazon EMR Studio.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon EMR Studio.
    studioArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Engine security group associated with the Amazon EMR
    -- Studio. The Engine security group allows inbound network traffic from
    -- resources in the Workspace security group.
    engineSecurityGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { creationTime = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      workspaceSecurityGroupId = Prelude.Nothing,
      defaultS3Location = Prelude.Nothing,
      authMode = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      userRole = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      url = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      studioArn = Prelude.Nothing,
      studioId = Prelude.Nothing,
      engineSecurityGroupId = Prelude.Nothing
    }

-- | The time the Amazon EMR Studio was created.
studio_creationTime :: Lens.Lens' Studio (Prelude.Maybe Prelude.UTCTime)
studio_creationTime = Lens.lens (\Studio' {creationTime} -> creationTime) (\s@Studio' {} a -> s {creationTime = a} :: Studio) Prelude.. Lens.mapping Prelude._Time

-- | The name of the IAM role assumed by the Amazon EMR Studio.
studio_serviceRole :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_serviceRole = Lens.lens (\Studio' {serviceRole} -> serviceRole) (\s@Studio' {} a -> s {serviceRole = a} :: Studio)

-- | The ID of the Workspace security group associated with the Amazon EMR
-- Studio. The Workspace security group allows outbound network traffic to
-- resources in the Engine security group and to the internet.
studio_workspaceSecurityGroupId :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_workspaceSecurityGroupId = Lens.lens (\Studio' {workspaceSecurityGroupId} -> workspaceSecurityGroupId) (\s@Studio' {} a -> s {workspaceSecurityGroupId = a} :: Studio)

-- | The default Amazon S3 location to back up Amazon EMR Studio Workspaces
-- and notebook files.
studio_defaultS3Location :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_defaultS3Location = Lens.lens (\Studio' {defaultS3Location} -> defaultS3Location) (\s@Studio' {} a -> s {defaultS3Location = a} :: Studio)

-- | Specifies whether the Amazon EMR Studio authenticates users using single
-- sign-on (SSO) or IAM.
studio_authMode :: Lens.Lens' Studio (Prelude.Maybe AuthMode)
studio_authMode = Lens.lens (\Studio' {authMode} -> authMode) (\s@Studio' {} a -> s {authMode = a} :: Studio)

-- | The list of IDs of the subnets associated with the Amazon EMR Studio.
studio_subnetIds :: Lens.Lens' Studio (Prelude.Maybe [Prelude.Text])
studio_subnetIds = Lens.lens (\Studio' {subnetIds} -> subnetIds) (\s@Studio' {} a -> s {subnetIds = a} :: Studio) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the IAM role assumed by users logged in to the Amazon EMR
-- Studio.
studio_userRole :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_userRole = Lens.lens (\Studio' {userRole} -> userRole) (\s@Studio' {} a -> s {userRole = a} :: Studio)

-- | The name of the Amazon EMR Studio.
studio_name :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_name = Lens.lens (\Studio' {name} -> name) (\s@Studio' {} a -> s {name = a} :: Studio)

-- | A list of tags associated with the Amazon EMR Studio.
studio_tags :: Lens.Lens' Studio (Prelude.Maybe [Tag])
studio_tags = Lens.lens (\Studio' {tags} -> tags) (\s@Studio' {} a -> s {tags = a} :: Studio) Prelude.. Lens.mapping Prelude._Coerce

-- | The detailed description of the Amazon EMR Studio.
studio_description :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_description = Lens.lens (\Studio' {description} -> description) (\s@Studio' {} a -> s {description = a} :: Studio)

-- | The unique access URL of the Amazon EMR Studio.
studio_url :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_url = Lens.lens (\Studio' {url} -> url) (\s@Studio' {} a -> s {url = a} :: Studio)

-- | The ID of the VPC associated with the Amazon EMR Studio.
studio_vpcId :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_vpcId = Lens.lens (\Studio' {vpcId} -> vpcId) (\s@Studio' {} a -> s {vpcId = a} :: Studio)

-- | The Amazon Resource Name (ARN) of the Amazon EMR Studio.
studio_studioArn :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_studioArn = Lens.lens (\Studio' {studioArn} -> studioArn) (\s@Studio' {} a -> s {studioArn = a} :: Studio)

-- | The ID of the Amazon EMR Studio.
studio_studioId :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_studioId = Lens.lens (\Studio' {studioId} -> studioId) (\s@Studio' {} a -> s {studioId = a} :: Studio)

-- | The ID of the Engine security group associated with the Amazon EMR
-- Studio. The Engine security group allows inbound network traffic from
-- resources in the Workspace security group.
studio_engineSecurityGroupId :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_engineSecurityGroupId = Lens.lens (\Studio' {engineSecurityGroupId} -> engineSecurityGroupId) (\s@Studio' {} a -> s {engineSecurityGroupId = a} :: Studio)

instance Prelude.FromJSON Studio where
  parseJSON =
    Prelude.withObject
      "Studio"
      ( \x ->
          Studio'
            Prelude.<$> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "ServiceRole")
            Prelude.<*> (x Prelude..:? "WorkspaceSecurityGroupId")
            Prelude.<*> (x Prelude..:? "DefaultS3Location")
            Prelude.<*> (x Prelude..:? "AuthMode")
            Prelude.<*> ( x Prelude..:? "SubnetIds"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "UserRole")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "Url")
            Prelude.<*> (x Prelude..:? "VpcId")
            Prelude.<*> (x Prelude..:? "StudioArn")
            Prelude.<*> (x Prelude..:? "StudioId")
            Prelude.<*> (x Prelude..:? "EngineSecurityGroupId")
      )

instance Prelude.Hashable Studio

instance Prelude.NFData Studio
