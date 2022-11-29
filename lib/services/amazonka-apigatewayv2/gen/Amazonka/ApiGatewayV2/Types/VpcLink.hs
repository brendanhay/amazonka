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
-- Module      : Amazonka.ApiGatewayV2.Types.VpcLink
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.VpcLink where

import Amazonka.ApiGatewayV2.Types.VpcLinkStatus
import Amazonka.ApiGatewayV2.Types.VpcLinkVersion
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a VPC link.
--
-- /See:/ 'newVpcLink' smart constructor.
data VpcLink = VpcLink'
  { -- | Tags for the VPC link.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A message summarizing the cause of the status of the VPC link.
    vpcLinkStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The version of the VPC link.
    vpcLinkVersion :: Prelude.Maybe VpcLinkVersion,
    -- | The status of the VPC link.
    vpcLinkStatus :: Prelude.Maybe VpcLinkStatus,
    -- | The timestamp when the VPC link was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The ID of the VPC link.
    vpcLinkId :: Prelude.Text,
    -- | A list of security group IDs for the VPC link.
    securityGroupIds :: [Prelude.Text],
    -- | A list of subnet IDs to include in the VPC link.
    subnetIds :: [Prelude.Text],
    -- | The name of the VPC link.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'vpcLink_tags' - Tags for the VPC link.
--
-- 'vpcLinkStatusMessage', 'vpcLink_vpcLinkStatusMessage' - A message summarizing the cause of the status of the VPC link.
--
-- 'vpcLinkVersion', 'vpcLink_vpcLinkVersion' - The version of the VPC link.
--
-- 'vpcLinkStatus', 'vpcLink_vpcLinkStatus' - The status of the VPC link.
--
-- 'createdDate', 'vpcLink_createdDate' - The timestamp when the VPC link was created.
--
-- 'vpcLinkId', 'vpcLink_vpcLinkId' - The ID of the VPC link.
--
-- 'securityGroupIds', 'vpcLink_securityGroupIds' - A list of security group IDs for the VPC link.
--
-- 'subnetIds', 'vpcLink_subnetIds' - A list of subnet IDs to include in the VPC link.
--
-- 'name', 'vpcLink_name' - The name of the VPC link.
newVpcLink ::
  -- | 'vpcLinkId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  VpcLink
newVpcLink pVpcLinkId_ pName_ =
  VpcLink'
    { tags = Prelude.Nothing,
      vpcLinkStatusMessage = Prelude.Nothing,
      vpcLinkVersion = Prelude.Nothing,
      vpcLinkStatus = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      vpcLinkId = pVpcLinkId_,
      securityGroupIds = Prelude.mempty,
      subnetIds = Prelude.mempty,
      name = pName_
    }

-- | Tags for the VPC link.
vpcLink_tags :: Lens.Lens' VpcLink (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
vpcLink_tags = Lens.lens (\VpcLink' {tags} -> tags) (\s@VpcLink' {} a -> s {tags = a} :: VpcLink) Prelude.. Lens.mapping Lens.coerced

-- | A message summarizing the cause of the status of the VPC link.
vpcLink_vpcLinkStatusMessage :: Lens.Lens' VpcLink (Prelude.Maybe Prelude.Text)
vpcLink_vpcLinkStatusMessage = Lens.lens (\VpcLink' {vpcLinkStatusMessage} -> vpcLinkStatusMessage) (\s@VpcLink' {} a -> s {vpcLinkStatusMessage = a} :: VpcLink)

-- | The version of the VPC link.
vpcLink_vpcLinkVersion :: Lens.Lens' VpcLink (Prelude.Maybe VpcLinkVersion)
vpcLink_vpcLinkVersion = Lens.lens (\VpcLink' {vpcLinkVersion} -> vpcLinkVersion) (\s@VpcLink' {} a -> s {vpcLinkVersion = a} :: VpcLink)

-- | The status of the VPC link.
vpcLink_vpcLinkStatus :: Lens.Lens' VpcLink (Prelude.Maybe VpcLinkStatus)
vpcLink_vpcLinkStatus = Lens.lens (\VpcLink' {vpcLinkStatus} -> vpcLinkStatus) (\s@VpcLink' {} a -> s {vpcLinkStatus = a} :: VpcLink)

-- | The timestamp when the VPC link was created.
vpcLink_createdDate :: Lens.Lens' VpcLink (Prelude.Maybe Prelude.UTCTime)
vpcLink_createdDate = Lens.lens (\VpcLink' {createdDate} -> createdDate) (\s@VpcLink' {} a -> s {createdDate = a} :: VpcLink) Prelude.. Lens.mapping Core._Time

-- | The ID of the VPC link.
vpcLink_vpcLinkId :: Lens.Lens' VpcLink Prelude.Text
vpcLink_vpcLinkId = Lens.lens (\VpcLink' {vpcLinkId} -> vpcLinkId) (\s@VpcLink' {} a -> s {vpcLinkId = a} :: VpcLink)

-- | A list of security group IDs for the VPC link.
vpcLink_securityGroupIds :: Lens.Lens' VpcLink [Prelude.Text]
vpcLink_securityGroupIds = Lens.lens (\VpcLink' {securityGroupIds} -> securityGroupIds) (\s@VpcLink' {} a -> s {securityGroupIds = a} :: VpcLink) Prelude.. Lens.coerced

-- | A list of subnet IDs to include in the VPC link.
vpcLink_subnetIds :: Lens.Lens' VpcLink [Prelude.Text]
vpcLink_subnetIds = Lens.lens (\VpcLink' {subnetIds} -> subnetIds) (\s@VpcLink' {} a -> s {subnetIds = a} :: VpcLink) Prelude.. Lens.coerced

-- | The name of the VPC link.
vpcLink_name :: Lens.Lens' VpcLink Prelude.Text
vpcLink_name = Lens.lens (\VpcLink' {name} -> name) (\s@VpcLink' {} a -> s {name = a} :: VpcLink)

instance Core.FromJSON VpcLink where
  parseJSON =
    Core.withObject
      "VpcLink"
      ( \x ->
          VpcLink'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "vpcLinkStatusMessage")
            Prelude.<*> (x Core..:? "vpcLinkVersion")
            Prelude.<*> (x Core..:? "vpcLinkStatus")
            Prelude.<*> (x Core..:? "createdDate")
            Prelude.<*> (x Core..: "vpcLinkId")
            Prelude.<*> ( x Core..:? "securityGroupIds"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "subnetIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable VpcLink where
  hashWithSalt _salt VpcLink' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcLinkStatusMessage
      `Prelude.hashWithSalt` vpcLinkVersion
      `Prelude.hashWithSalt` vpcLinkStatus
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` vpcLinkId
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` name

instance Prelude.NFData VpcLink where
  rnf VpcLink' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcLinkStatusMessage
      `Prelude.seq` Prelude.rnf vpcLinkVersion
      `Prelude.seq` Prelude.rnf vpcLinkStatus
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf vpcLinkId
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf name
