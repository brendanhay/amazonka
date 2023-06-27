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
-- Module      : Amazonka.QuickSight.Types.AssetBundleImportJobVPCConnectionOverrideParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleImportJobVPCConnectionOverrideParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The override parameters for a single VPC connection that is imported.
--
-- /See:/ 'newAssetBundleImportJobVPCConnectionOverrideParameters' smart constructor.
data AssetBundleImportJobVPCConnectionOverrideParameters = AssetBundleImportJobVPCConnectionOverrideParameters'
  { -- | An optional override of DNS resolvers to be used by the VPC connection.
    dnsResolvers :: Prelude.Maybe [Prelude.Text],
    -- | A new name for the VPC connection.
    name :: Prelude.Maybe Prelude.Text,
    -- | An optional override of the role ARN to be used by the VPC connection.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | A new security group ID for the VPC connection you are importing. This
    -- field is required if you are importing the VPC connection from another
    -- Amazon Web Services account or Region.
    securityGroupIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of new subnet IDs for the VPC connection you are importing. This
    -- field is required if you are importing the VPC connection from another
    -- Amazon Web Services account or Region.
    subnetIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The ID of the VPC Connection to apply overrides to.
    vPCConnectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleImportJobVPCConnectionOverrideParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsResolvers', 'assetBundleImportJobVPCConnectionOverrideParameters_dnsResolvers' - An optional override of DNS resolvers to be used by the VPC connection.
--
-- 'name', 'assetBundleImportJobVPCConnectionOverrideParameters_name' - A new name for the VPC connection.
--
-- 'roleArn', 'assetBundleImportJobVPCConnectionOverrideParameters_roleArn' - An optional override of the role ARN to be used by the VPC connection.
--
-- 'securityGroupIds', 'assetBundleImportJobVPCConnectionOverrideParameters_securityGroupIds' - A new security group ID for the VPC connection you are importing. This
-- field is required if you are importing the VPC connection from another
-- Amazon Web Services account or Region.
--
-- 'subnetIds', 'assetBundleImportJobVPCConnectionOverrideParameters_subnetIds' - A list of new subnet IDs for the VPC connection you are importing. This
-- field is required if you are importing the VPC connection from another
-- Amazon Web Services account or Region.
--
-- 'vPCConnectionId', 'assetBundleImportJobVPCConnectionOverrideParameters_vPCConnectionId' - The ID of the VPC Connection to apply overrides to.
newAssetBundleImportJobVPCConnectionOverrideParameters ::
  -- | 'vPCConnectionId'
  Prelude.Text ->
  AssetBundleImportJobVPCConnectionOverrideParameters
newAssetBundleImportJobVPCConnectionOverrideParameters
  pVPCConnectionId_ =
    AssetBundleImportJobVPCConnectionOverrideParameters'
      { dnsResolvers =
          Prelude.Nothing,
        name = Prelude.Nothing,
        roleArn =
          Prelude.Nothing,
        securityGroupIds =
          Prelude.Nothing,
        subnetIds =
          Prelude.Nothing,
        vPCConnectionId =
          pVPCConnectionId_
      }

-- | An optional override of DNS resolvers to be used by the VPC connection.
assetBundleImportJobVPCConnectionOverrideParameters_dnsResolvers :: Lens.Lens' AssetBundleImportJobVPCConnectionOverrideParameters (Prelude.Maybe [Prelude.Text])
assetBundleImportJobVPCConnectionOverrideParameters_dnsResolvers = Lens.lens (\AssetBundleImportJobVPCConnectionOverrideParameters' {dnsResolvers} -> dnsResolvers) (\s@AssetBundleImportJobVPCConnectionOverrideParameters' {} a -> s {dnsResolvers = a} :: AssetBundleImportJobVPCConnectionOverrideParameters) Prelude.. Lens.mapping Lens.coerced

-- | A new name for the VPC connection.
assetBundleImportJobVPCConnectionOverrideParameters_name :: Lens.Lens' AssetBundleImportJobVPCConnectionOverrideParameters (Prelude.Maybe Prelude.Text)
assetBundleImportJobVPCConnectionOverrideParameters_name = Lens.lens (\AssetBundleImportJobVPCConnectionOverrideParameters' {name} -> name) (\s@AssetBundleImportJobVPCConnectionOverrideParameters' {} a -> s {name = a} :: AssetBundleImportJobVPCConnectionOverrideParameters)

-- | An optional override of the role ARN to be used by the VPC connection.
assetBundleImportJobVPCConnectionOverrideParameters_roleArn :: Lens.Lens' AssetBundleImportJobVPCConnectionOverrideParameters (Prelude.Maybe Prelude.Text)
assetBundleImportJobVPCConnectionOverrideParameters_roleArn = Lens.lens (\AssetBundleImportJobVPCConnectionOverrideParameters' {roleArn} -> roleArn) (\s@AssetBundleImportJobVPCConnectionOverrideParameters' {} a -> s {roleArn = a} :: AssetBundleImportJobVPCConnectionOverrideParameters)

-- | A new security group ID for the VPC connection you are importing. This
-- field is required if you are importing the VPC connection from another
-- Amazon Web Services account or Region.
assetBundleImportJobVPCConnectionOverrideParameters_securityGroupIds :: Lens.Lens' AssetBundleImportJobVPCConnectionOverrideParameters (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
assetBundleImportJobVPCConnectionOverrideParameters_securityGroupIds = Lens.lens (\AssetBundleImportJobVPCConnectionOverrideParameters' {securityGroupIds} -> securityGroupIds) (\s@AssetBundleImportJobVPCConnectionOverrideParameters' {} a -> s {securityGroupIds = a} :: AssetBundleImportJobVPCConnectionOverrideParameters) Prelude.. Lens.mapping Lens.coerced

-- | A list of new subnet IDs for the VPC connection you are importing. This
-- field is required if you are importing the VPC connection from another
-- Amazon Web Services account or Region.
assetBundleImportJobVPCConnectionOverrideParameters_subnetIds :: Lens.Lens' AssetBundleImportJobVPCConnectionOverrideParameters (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
assetBundleImportJobVPCConnectionOverrideParameters_subnetIds = Lens.lens (\AssetBundleImportJobVPCConnectionOverrideParameters' {subnetIds} -> subnetIds) (\s@AssetBundleImportJobVPCConnectionOverrideParameters' {} a -> s {subnetIds = a} :: AssetBundleImportJobVPCConnectionOverrideParameters) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC Connection to apply overrides to.
assetBundleImportJobVPCConnectionOverrideParameters_vPCConnectionId :: Lens.Lens' AssetBundleImportJobVPCConnectionOverrideParameters Prelude.Text
assetBundleImportJobVPCConnectionOverrideParameters_vPCConnectionId = Lens.lens (\AssetBundleImportJobVPCConnectionOverrideParameters' {vPCConnectionId} -> vPCConnectionId) (\s@AssetBundleImportJobVPCConnectionOverrideParameters' {} a -> s {vPCConnectionId = a} :: AssetBundleImportJobVPCConnectionOverrideParameters)

instance
  Data.FromJSON
    AssetBundleImportJobVPCConnectionOverrideParameters
  where
  parseJSON =
    Data.withObject
      "AssetBundleImportJobVPCConnectionOverrideParameters"
      ( \x ->
          AssetBundleImportJobVPCConnectionOverrideParameters'
            Prelude.<$> (x Data..:? "DnsResolvers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "SecurityGroupIds")
            Prelude.<*> (x Data..:? "SubnetIds")
            Prelude.<*> (x Data..: "VPCConnectionId")
      )

instance
  Prelude.Hashable
    AssetBundleImportJobVPCConnectionOverrideParameters
  where
  hashWithSalt
    _salt
    AssetBundleImportJobVPCConnectionOverrideParameters' {..} =
      _salt
        `Prelude.hashWithSalt` dnsResolvers
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` securityGroupIds
        `Prelude.hashWithSalt` subnetIds
        `Prelude.hashWithSalt` vPCConnectionId

instance
  Prelude.NFData
    AssetBundleImportJobVPCConnectionOverrideParameters
  where
  rnf
    AssetBundleImportJobVPCConnectionOverrideParameters' {..} =
      Prelude.rnf dnsResolvers
        `Prelude.seq` Prelude.rnf name
        `Prelude.seq` Prelude.rnf roleArn
        `Prelude.seq` Prelude.rnf securityGroupIds
        `Prelude.seq` Prelude.rnf subnetIds
        `Prelude.seq` Prelude.rnf vPCConnectionId

instance
  Data.ToJSON
    AssetBundleImportJobVPCConnectionOverrideParameters
  where
  toJSON
    AssetBundleImportJobVPCConnectionOverrideParameters' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("DnsResolvers" Data..=) Prelude.<$> dnsResolvers,
              ("Name" Data..=) Prelude.<$> name,
              ("RoleArn" Data..=) Prelude.<$> roleArn,
              ("SecurityGroupIds" Data..=)
                Prelude.<$> securityGroupIds,
              ("SubnetIds" Data..=) Prelude.<$> subnetIds,
              Prelude.Just
                ("VPCConnectionId" Data..= vPCConnectionId)
            ]
        )
