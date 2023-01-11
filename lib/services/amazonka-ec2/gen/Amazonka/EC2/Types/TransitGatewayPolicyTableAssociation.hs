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
-- Module      : Amazonka.EC2.Types.TransitGatewayPolicyTableAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayPolicyTableAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.TransitGatewayAssociationState
import Amazonka.EC2.Types.TransitGatewayAttachmentResourceType
import qualified Amazonka.Prelude as Prelude

-- | Describes a transit gateway policy table association.
--
-- /See:/ 'newTransitGatewayPolicyTableAssociation' smart constructor.
data TransitGatewayPolicyTableAssociation = TransitGatewayPolicyTableAssociation'
  { -- | The resource ID of the transit gateway attachment.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The resource type for the transit gateway policy table association.
    resourceType :: Prelude.Maybe TransitGatewayAttachmentResourceType,
    -- | The state of the transit gateway policy table association.
    state :: Prelude.Maybe TransitGatewayAssociationState,
    -- | The ID of the transit gateway attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway policy table.
    transitGatewayPolicyTableId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayPolicyTableAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'transitGatewayPolicyTableAssociation_resourceId' - The resource ID of the transit gateway attachment.
--
-- 'resourceType', 'transitGatewayPolicyTableAssociation_resourceType' - The resource type for the transit gateway policy table association.
--
-- 'state', 'transitGatewayPolicyTableAssociation_state' - The state of the transit gateway policy table association.
--
-- 'transitGatewayAttachmentId', 'transitGatewayPolicyTableAssociation_transitGatewayAttachmentId' - The ID of the transit gateway attachment.
--
-- 'transitGatewayPolicyTableId', 'transitGatewayPolicyTableAssociation_transitGatewayPolicyTableId' - The ID of the transit gateway policy table.
newTransitGatewayPolicyTableAssociation ::
  TransitGatewayPolicyTableAssociation
newTransitGatewayPolicyTableAssociation =
  TransitGatewayPolicyTableAssociation'
    { resourceId =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      state = Prelude.Nothing,
      transitGatewayAttachmentId =
        Prelude.Nothing,
      transitGatewayPolicyTableId =
        Prelude.Nothing
    }

-- | The resource ID of the transit gateway attachment.
transitGatewayPolicyTableAssociation_resourceId :: Lens.Lens' TransitGatewayPolicyTableAssociation (Prelude.Maybe Prelude.Text)
transitGatewayPolicyTableAssociation_resourceId = Lens.lens (\TransitGatewayPolicyTableAssociation' {resourceId} -> resourceId) (\s@TransitGatewayPolicyTableAssociation' {} a -> s {resourceId = a} :: TransitGatewayPolicyTableAssociation)

-- | The resource type for the transit gateway policy table association.
transitGatewayPolicyTableAssociation_resourceType :: Lens.Lens' TransitGatewayPolicyTableAssociation (Prelude.Maybe TransitGatewayAttachmentResourceType)
transitGatewayPolicyTableAssociation_resourceType = Lens.lens (\TransitGatewayPolicyTableAssociation' {resourceType} -> resourceType) (\s@TransitGatewayPolicyTableAssociation' {} a -> s {resourceType = a} :: TransitGatewayPolicyTableAssociation)

-- | The state of the transit gateway policy table association.
transitGatewayPolicyTableAssociation_state :: Lens.Lens' TransitGatewayPolicyTableAssociation (Prelude.Maybe TransitGatewayAssociationState)
transitGatewayPolicyTableAssociation_state = Lens.lens (\TransitGatewayPolicyTableAssociation' {state} -> state) (\s@TransitGatewayPolicyTableAssociation' {} a -> s {state = a} :: TransitGatewayPolicyTableAssociation)

-- | The ID of the transit gateway attachment.
transitGatewayPolicyTableAssociation_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayPolicyTableAssociation (Prelude.Maybe Prelude.Text)
transitGatewayPolicyTableAssociation_transitGatewayAttachmentId = Lens.lens (\TransitGatewayPolicyTableAssociation' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayPolicyTableAssociation' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayPolicyTableAssociation)

-- | The ID of the transit gateway policy table.
transitGatewayPolicyTableAssociation_transitGatewayPolicyTableId :: Lens.Lens' TransitGatewayPolicyTableAssociation (Prelude.Maybe Prelude.Text)
transitGatewayPolicyTableAssociation_transitGatewayPolicyTableId = Lens.lens (\TransitGatewayPolicyTableAssociation' {transitGatewayPolicyTableId} -> transitGatewayPolicyTableId) (\s@TransitGatewayPolicyTableAssociation' {} a -> s {transitGatewayPolicyTableId = a} :: TransitGatewayPolicyTableAssociation)

instance
  Data.FromXML
    TransitGatewayPolicyTableAssociation
  where
  parseXML x =
    TransitGatewayPolicyTableAssociation'
      Prelude.<$> (x Data..@? "resourceId")
      Prelude.<*> (x Data..@? "resourceType")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "transitGatewayAttachmentId")
      Prelude.<*> (x Data..@? "transitGatewayPolicyTableId")

instance
  Prelude.Hashable
    TransitGatewayPolicyTableAssociation
  where
  hashWithSalt
    _salt
    TransitGatewayPolicyTableAssociation' {..} =
      _salt `Prelude.hashWithSalt` resourceId
        `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` state
        `Prelude.hashWithSalt` transitGatewayAttachmentId
        `Prelude.hashWithSalt` transitGatewayPolicyTableId

instance
  Prelude.NFData
    TransitGatewayPolicyTableAssociation
  where
  rnf TransitGatewayPolicyTableAssociation' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId
      `Prelude.seq` Prelude.rnf transitGatewayPolicyTableId
