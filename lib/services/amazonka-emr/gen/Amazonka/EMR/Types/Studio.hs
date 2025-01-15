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
-- Module      : Amazonka.EMR.Types.Studio
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.Studio where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.AuthMode
import Amazonka.EMR.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Details for an Amazon EMR Studio including ID, creation time, name, and
-- so on.
--
-- /See:/ 'newStudio' smart constructor.
data Studio = Studio'
  { -- | Specifies whether the Amazon EMR Studio authenticates users using IAM or
    -- IAM Identity Center.
    authMode :: Prelude.Maybe AuthMode,
    -- | The time the Amazon EMR Studio was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon S3 location to back up Amazon EMR Studio Workspaces and
    -- notebook files.
    defaultS3Location :: Prelude.Maybe Prelude.Text,
    -- | The detailed description of the Amazon EMR Studio.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Engine security group associated with the Amazon EMR
    -- Studio. The Engine security group allows inbound network traffic from
    -- resources in the Workspace security group.
    engineSecurityGroupId :: Prelude.Maybe Prelude.Text,
    -- | Your identity provider\'s authentication endpoint. Amazon EMR Studio
    -- redirects federated users to this endpoint for authentication when
    -- logging in to a Studio with the Studio URL.
    idpAuthUrl :: Prelude.Maybe Prelude.Text,
    -- | The name of your identity provider\'s @RelayState@ parameter.
    idpRelayStateParameterName :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon EMR Studio.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the IAM role assumed by the Amazon EMR Studio.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon EMR Studio.
    studioArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Prelude.Maybe Prelude.Text,
    -- | The list of IDs of the subnets associated with the Amazon EMR Studio.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of tags associated with the Amazon EMR Studio.
    tags :: Prelude.Maybe [Tag],
    -- | The unique access URL of the Amazon EMR Studio.
    url :: Prelude.Maybe Prelude.Text,
    -- | The name of the IAM role assumed by users logged in to the Amazon EMR
    -- Studio. A Studio only requires a @UserRole@ when you use IAM
    -- authentication.
    userRole :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC associated with the Amazon EMR Studio.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Workspace security group associated with the Amazon EMR
    -- Studio. The Workspace security group allows outbound network traffic to
    -- resources in the Engine security group and to the internet.
    workspaceSecurityGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Studio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authMode', 'studio_authMode' - Specifies whether the Amazon EMR Studio authenticates users using IAM or
-- IAM Identity Center.
--
-- 'creationTime', 'studio_creationTime' - The time the Amazon EMR Studio was created.
--
-- 'defaultS3Location', 'studio_defaultS3Location' - The Amazon S3 location to back up Amazon EMR Studio Workspaces and
-- notebook files.
--
-- 'description', 'studio_description' - The detailed description of the Amazon EMR Studio.
--
-- 'engineSecurityGroupId', 'studio_engineSecurityGroupId' - The ID of the Engine security group associated with the Amazon EMR
-- Studio. The Engine security group allows inbound network traffic from
-- resources in the Workspace security group.
--
-- 'idpAuthUrl', 'studio_idpAuthUrl' - Your identity provider\'s authentication endpoint. Amazon EMR Studio
-- redirects federated users to this endpoint for authentication when
-- logging in to a Studio with the Studio URL.
--
-- 'idpRelayStateParameterName', 'studio_idpRelayStateParameterName' - The name of your identity provider\'s @RelayState@ parameter.
--
-- 'name', 'studio_name' - The name of the Amazon EMR Studio.
--
-- 'serviceRole', 'studio_serviceRole' - The name of the IAM role assumed by the Amazon EMR Studio.
--
-- 'studioArn', 'studio_studioArn' - The Amazon Resource Name (ARN) of the Amazon EMR Studio.
--
-- 'studioId', 'studio_studioId' - The ID of the Amazon EMR Studio.
--
-- 'subnetIds', 'studio_subnetIds' - The list of IDs of the subnets associated with the Amazon EMR Studio.
--
-- 'tags', 'studio_tags' - A list of tags associated with the Amazon EMR Studio.
--
-- 'url', 'studio_url' - The unique access URL of the Amazon EMR Studio.
--
-- 'userRole', 'studio_userRole' - The name of the IAM role assumed by users logged in to the Amazon EMR
-- Studio. A Studio only requires a @UserRole@ when you use IAM
-- authentication.
--
-- 'vpcId', 'studio_vpcId' - The ID of the VPC associated with the Amazon EMR Studio.
--
-- 'workspaceSecurityGroupId', 'studio_workspaceSecurityGroupId' - The ID of the Workspace security group associated with the Amazon EMR
-- Studio. The Workspace security group allows outbound network traffic to
-- resources in the Engine security group and to the internet.
newStudio ::
  Studio
newStudio =
  Studio'
    { authMode = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      defaultS3Location = Prelude.Nothing,
      description = Prelude.Nothing,
      engineSecurityGroupId = Prelude.Nothing,
      idpAuthUrl = Prelude.Nothing,
      idpRelayStateParameterName = Prelude.Nothing,
      name = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      studioArn = Prelude.Nothing,
      studioId = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      tags = Prelude.Nothing,
      url = Prelude.Nothing,
      userRole = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      workspaceSecurityGroupId = Prelude.Nothing
    }

-- | Specifies whether the Amazon EMR Studio authenticates users using IAM or
-- IAM Identity Center.
studio_authMode :: Lens.Lens' Studio (Prelude.Maybe AuthMode)
studio_authMode = Lens.lens (\Studio' {authMode} -> authMode) (\s@Studio' {} a -> s {authMode = a} :: Studio)

-- | The time the Amazon EMR Studio was created.
studio_creationTime :: Lens.Lens' Studio (Prelude.Maybe Prelude.UTCTime)
studio_creationTime = Lens.lens (\Studio' {creationTime} -> creationTime) (\s@Studio' {} a -> s {creationTime = a} :: Studio) Prelude.. Lens.mapping Data._Time

-- | The Amazon S3 location to back up Amazon EMR Studio Workspaces and
-- notebook files.
studio_defaultS3Location :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_defaultS3Location = Lens.lens (\Studio' {defaultS3Location} -> defaultS3Location) (\s@Studio' {} a -> s {defaultS3Location = a} :: Studio)

-- | The detailed description of the Amazon EMR Studio.
studio_description :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_description = Lens.lens (\Studio' {description} -> description) (\s@Studio' {} a -> s {description = a} :: Studio)

-- | The ID of the Engine security group associated with the Amazon EMR
-- Studio. The Engine security group allows inbound network traffic from
-- resources in the Workspace security group.
studio_engineSecurityGroupId :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_engineSecurityGroupId = Lens.lens (\Studio' {engineSecurityGroupId} -> engineSecurityGroupId) (\s@Studio' {} a -> s {engineSecurityGroupId = a} :: Studio)

-- | Your identity provider\'s authentication endpoint. Amazon EMR Studio
-- redirects federated users to this endpoint for authentication when
-- logging in to a Studio with the Studio URL.
studio_idpAuthUrl :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_idpAuthUrl = Lens.lens (\Studio' {idpAuthUrl} -> idpAuthUrl) (\s@Studio' {} a -> s {idpAuthUrl = a} :: Studio)

-- | The name of your identity provider\'s @RelayState@ parameter.
studio_idpRelayStateParameterName :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_idpRelayStateParameterName = Lens.lens (\Studio' {idpRelayStateParameterName} -> idpRelayStateParameterName) (\s@Studio' {} a -> s {idpRelayStateParameterName = a} :: Studio)

-- | The name of the Amazon EMR Studio.
studio_name :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_name = Lens.lens (\Studio' {name} -> name) (\s@Studio' {} a -> s {name = a} :: Studio)

-- | The name of the IAM role assumed by the Amazon EMR Studio.
studio_serviceRole :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_serviceRole = Lens.lens (\Studio' {serviceRole} -> serviceRole) (\s@Studio' {} a -> s {serviceRole = a} :: Studio)

-- | The Amazon Resource Name (ARN) of the Amazon EMR Studio.
studio_studioArn :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_studioArn = Lens.lens (\Studio' {studioArn} -> studioArn) (\s@Studio' {} a -> s {studioArn = a} :: Studio)

-- | The ID of the Amazon EMR Studio.
studio_studioId :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_studioId = Lens.lens (\Studio' {studioId} -> studioId) (\s@Studio' {} a -> s {studioId = a} :: Studio)

-- | The list of IDs of the subnets associated with the Amazon EMR Studio.
studio_subnetIds :: Lens.Lens' Studio (Prelude.Maybe [Prelude.Text])
studio_subnetIds = Lens.lens (\Studio' {subnetIds} -> subnetIds) (\s@Studio' {} a -> s {subnetIds = a} :: Studio) Prelude.. Lens.mapping Lens.coerced

-- | A list of tags associated with the Amazon EMR Studio.
studio_tags :: Lens.Lens' Studio (Prelude.Maybe [Tag])
studio_tags = Lens.lens (\Studio' {tags} -> tags) (\s@Studio' {} a -> s {tags = a} :: Studio) Prelude.. Lens.mapping Lens.coerced

-- | The unique access URL of the Amazon EMR Studio.
studio_url :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_url = Lens.lens (\Studio' {url} -> url) (\s@Studio' {} a -> s {url = a} :: Studio)

-- | The name of the IAM role assumed by users logged in to the Amazon EMR
-- Studio. A Studio only requires a @UserRole@ when you use IAM
-- authentication.
studio_userRole :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_userRole = Lens.lens (\Studio' {userRole} -> userRole) (\s@Studio' {} a -> s {userRole = a} :: Studio)

-- | The ID of the VPC associated with the Amazon EMR Studio.
studio_vpcId :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_vpcId = Lens.lens (\Studio' {vpcId} -> vpcId) (\s@Studio' {} a -> s {vpcId = a} :: Studio)

-- | The ID of the Workspace security group associated with the Amazon EMR
-- Studio. The Workspace security group allows outbound network traffic to
-- resources in the Engine security group and to the internet.
studio_workspaceSecurityGroupId :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_workspaceSecurityGroupId = Lens.lens (\Studio' {workspaceSecurityGroupId} -> workspaceSecurityGroupId) (\s@Studio' {} a -> s {workspaceSecurityGroupId = a} :: Studio)

instance Data.FromJSON Studio where
  parseJSON =
    Data.withObject
      "Studio"
      ( \x ->
          Studio'
            Prelude.<$> (x Data..:? "AuthMode")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DefaultS3Location")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EngineSecurityGroupId")
            Prelude.<*> (x Data..:? "IdpAuthUrl")
            Prelude.<*> (x Data..:? "IdpRelayStateParameterName")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ServiceRole")
            Prelude.<*> (x Data..:? "StudioArn")
            Prelude.<*> (x Data..:? "StudioId")
            Prelude.<*> (x Data..:? "SubnetIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Url")
            Prelude.<*> (x Data..:? "UserRole")
            Prelude.<*> (x Data..:? "VpcId")
            Prelude.<*> (x Data..:? "WorkspaceSecurityGroupId")
      )

instance Prelude.Hashable Studio where
  hashWithSalt _salt Studio' {..} =
    _salt
      `Prelude.hashWithSalt` authMode
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` defaultS3Location
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` engineSecurityGroupId
      `Prelude.hashWithSalt` idpAuthUrl
      `Prelude.hashWithSalt` idpRelayStateParameterName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` serviceRole
      `Prelude.hashWithSalt` studioArn
      `Prelude.hashWithSalt` studioId
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` userRole
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` workspaceSecurityGroupId

instance Prelude.NFData Studio where
  rnf Studio' {..} =
    Prelude.rnf authMode `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf defaultS3Location `Prelude.seq`
          Prelude.rnf description `Prelude.seq`
            Prelude.rnf engineSecurityGroupId `Prelude.seq`
              Prelude.rnf idpAuthUrl `Prelude.seq`
                Prelude.rnf idpRelayStateParameterName `Prelude.seq`
                  Prelude.rnf name `Prelude.seq`
                    Prelude.rnf serviceRole `Prelude.seq`
                      Prelude.rnf studioArn `Prelude.seq`
                        Prelude.rnf studioId `Prelude.seq`
                          Prelude.rnf subnetIds `Prelude.seq`
                            Prelude.rnf tags `Prelude.seq`
                              Prelude.rnf url `Prelude.seq`
                                Prelude.rnf userRole `Prelude.seq`
                                  Prelude.rnf vpcId `Prelude.seq`
                                    Prelude.rnf workspaceSecurityGroupId
