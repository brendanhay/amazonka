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
-- Module      : Amazonka.EC2.Types.IpamResourceDiscoveryAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamResourceDiscoveryAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.IpamAssociatedResourceDiscoveryStatus
import Amazonka.EC2.Types.IpamResourceDiscoveryAssociationState
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | An IPAM resource discovery association. An associated resource discovery
-- is a resource discovery that has been associated with an IPAM. IPAM
-- aggregates the resource CIDRs discovered by the associated resource
-- discovery.
--
-- /See:/ 'newIpamResourceDiscoveryAssociation' smart constructor.
data IpamResourceDiscoveryAssociation = IpamResourceDiscoveryAssociation'
  { -- | The IPAM ARN.
    ipamArn :: Prelude.Maybe Prelude.Text,
    -- | The IPAM ID.
    ipamId :: Prelude.Maybe Prelude.Text,
    -- | The IPAM home Region.
    ipamRegion :: Prelude.Maybe Prelude.Text,
    -- | The resource discovery association Amazon Resource Name (ARN).
    ipamResourceDiscoveryAssociationArn :: Prelude.Maybe Prelude.Text,
    -- | The resource discovery association ID.
    ipamResourceDiscoveryAssociationId :: Prelude.Maybe Prelude.Text,
    -- | The resource discovery ID.
    ipamResourceDiscoveryId :: Prelude.Maybe Prelude.Text,
    -- | Defines if the resource discovery is the default. When you create an
    -- IPAM, a default resource discovery is created for your IPAM and it\'s
    -- associated with your IPAM.
    isDefault :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services account ID of the resource discovery owner.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The resource discovery status.
    --
    -- -   @active@ - Connection or permissions required to read the results of
    --     the resource discovery are intact.
    --
    -- -   @not-found@ - Connection or permissions required to read the results
    --     of the resource discovery are broken. This may happen if the owner
    --     of the resource discovery stopped sharing it or deleted the resource
    --     discovery. Verify the resource discovery still exists and the Amazon
    --     Web Services RAM resource share is still intact.
    resourceDiscoveryStatus :: Prelude.Maybe IpamAssociatedResourceDiscoveryStatus,
    -- | The lifecycle state of the association when you associate or
    -- disassociate a resource discovery.
    --
    -- -   @associate-in-progress@ - Resource discovery is being associated.
    --
    -- -   @associate-complete@ - Resource discovery association is complete.
    --
    -- -   @associate-failed@ - Resource discovery association has failed.
    --
    -- -   @disassociate-in-progress@ - Resource discovery is being
    --     disassociated.
    --
    -- -   @disassociate-complete@ - Resource discovery disassociation is
    --     complete.
    --
    -- -   @disassociate-failed @ - Resource discovery disassociation has
    --     failed.
    --
    -- -   @isolate-in-progress@ - Amazon Web Services account that created the
    --     resource discovery association has been removed and the resource
    --     discovery associatation is being isolated.
    --
    -- -   @isolate-complete@ - Resource discovery isolation is complete..
    --
    -- -   @restore-in-progress@ - Resource discovery is being restored.
    state :: Prelude.Maybe IpamResourceDiscoveryAssociationState,
    -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value. You can use tags to
    -- search and filter your resources or track your Amazon Web Services
    -- costs.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpamResourceDiscoveryAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamArn', 'ipamResourceDiscoveryAssociation_ipamArn' - The IPAM ARN.
--
-- 'ipamId', 'ipamResourceDiscoveryAssociation_ipamId' - The IPAM ID.
--
-- 'ipamRegion', 'ipamResourceDiscoveryAssociation_ipamRegion' - The IPAM home Region.
--
-- 'ipamResourceDiscoveryAssociationArn', 'ipamResourceDiscoveryAssociation_ipamResourceDiscoveryAssociationArn' - The resource discovery association Amazon Resource Name (ARN).
--
-- 'ipamResourceDiscoveryAssociationId', 'ipamResourceDiscoveryAssociation_ipamResourceDiscoveryAssociationId' - The resource discovery association ID.
--
-- 'ipamResourceDiscoveryId', 'ipamResourceDiscoveryAssociation_ipamResourceDiscoveryId' - The resource discovery ID.
--
-- 'isDefault', 'ipamResourceDiscoveryAssociation_isDefault' - Defines if the resource discovery is the default. When you create an
-- IPAM, a default resource discovery is created for your IPAM and it\'s
-- associated with your IPAM.
--
-- 'ownerId', 'ipamResourceDiscoveryAssociation_ownerId' - The Amazon Web Services account ID of the resource discovery owner.
--
-- 'resourceDiscoveryStatus', 'ipamResourceDiscoveryAssociation_resourceDiscoveryStatus' - The resource discovery status.
--
-- -   @active@ - Connection or permissions required to read the results of
--     the resource discovery are intact.
--
-- -   @not-found@ - Connection or permissions required to read the results
--     of the resource discovery are broken. This may happen if the owner
--     of the resource discovery stopped sharing it or deleted the resource
--     discovery. Verify the resource discovery still exists and the Amazon
--     Web Services RAM resource share is still intact.
--
-- 'state', 'ipamResourceDiscoveryAssociation_state' - The lifecycle state of the association when you associate or
-- disassociate a resource discovery.
--
-- -   @associate-in-progress@ - Resource discovery is being associated.
--
-- -   @associate-complete@ - Resource discovery association is complete.
--
-- -   @associate-failed@ - Resource discovery association has failed.
--
-- -   @disassociate-in-progress@ - Resource discovery is being
--     disassociated.
--
-- -   @disassociate-complete@ - Resource discovery disassociation is
--     complete.
--
-- -   @disassociate-failed @ - Resource discovery disassociation has
--     failed.
--
-- -   @isolate-in-progress@ - Amazon Web Services account that created the
--     resource discovery association has been removed and the resource
--     discovery associatation is being isolated.
--
-- -   @isolate-complete@ - Resource discovery isolation is complete..
--
-- -   @restore-in-progress@ - Resource discovery is being restored.
--
-- 'tags', 'ipamResourceDiscoveryAssociation_tags' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
newIpamResourceDiscoveryAssociation ::
  IpamResourceDiscoveryAssociation
newIpamResourceDiscoveryAssociation =
  IpamResourceDiscoveryAssociation'
    { ipamArn =
        Prelude.Nothing,
      ipamId = Prelude.Nothing,
      ipamRegion = Prelude.Nothing,
      ipamResourceDiscoveryAssociationArn =
        Prelude.Nothing,
      ipamResourceDiscoveryAssociationId =
        Prelude.Nothing,
      ipamResourceDiscoveryId = Prelude.Nothing,
      isDefault = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      resourceDiscoveryStatus = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The IPAM ARN.
ipamResourceDiscoveryAssociation_ipamArn :: Lens.Lens' IpamResourceDiscoveryAssociation (Prelude.Maybe Prelude.Text)
ipamResourceDiscoveryAssociation_ipamArn = Lens.lens (\IpamResourceDiscoveryAssociation' {ipamArn} -> ipamArn) (\s@IpamResourceDiscoveryAssociation' {} a -> s {ipamArn = a} :: IpamResourceDiscoveryAssociation)

-- | The IPAM ID.
ipamResourceDiscoveryAssociation_ipamId :: Lens.Lens' IpamResourceDiscoveryAssociation (Prelude.Maybe Prelude.Text)
ipamResourceDiscoveryAssociation_ipamId = Lens.lens (\IpamResourceDiscoveryAssociation' {ipamId} -> ipamId) (\s@IpamResourceDiscoveryAssociation' {} a -> s {ipamId = a} :: IpamResourceDiscoveryAssociation)

-- | The IPAM home Region.
ipamResourceDiscoveryAssociation_ipamRegion :: Lens.Lens' IpamResourceDiscoveryAssociation (Prelude.Maybe Prelude.Text)
ipamResourceDiscoveryAssociation_ipamRegion = Lens.lens (\IpamResourceDiscoveryAssociation' {ipamRegion} -> ipamRegion) (\s@IpamResourceDiscoveryAssociation' {} a -> s {ipamRegion = a} :: IpamResourceDiscoveryAssociation)

-- | The resource discovery association Amazon Resource Name (ARN).
ipamResourceDiscoveryAssociation_ipamResourceDiscoveryAssociationArn :: Lens.Lens' IpamResourceDiscoveryAssociation (Prelude.Maybe Prelude.Text)
ipamResourceDiscoveryAssociation_ipamResourceDiscoveryAssociationArn = Lens.lens (\IpamResourceDiscoveryAssociation' {ipamResourceDiscoveryAssociationArn} -> ipamResourceDiscoveryAssociationArn) (\s@IpamResourceDiscoveryAssociation' {} a -> s {ipamResourceDiscoveryAssociationArn = a} :: IpamResourceDiscoveryAssociation)

-- | The resource discovery association ID.
ipamResourceDiscoveryAssociation_ipamResourceDiscoveryAssociationId :: Lens.Lens' IpamResourceDiscoveryAssociation (Prelude.Maybe Prelude.Text)
ipamResourceDiscoveryAssociation_ipamResourceDiscoveryAssociationId = Lens.lens (\IpamResourceDiscoveryAssociation' {ipamResourceDiscoveryAssociationId} -> ipamResourceDiscoveryAssociationId) (\s@IpamResourceDiscoveryAssociation' {} a -> s {ipamResourceDiscoveryAssociationId = a} :: IpamResourceDiscoveryAssociation)

-- | The resource discovery ID.
ipamResourceDiscoveryAssociation_ipamResourceDiscoveryId :: Lens.Lens' IpamResourceDiscoveryAssociation (Prelude.Maybe Prelude.Text)
ipamResourceDiscoveryAssociation_ipamResourceDiscoveryId = Lens.lens (\IpamResourceDiscoveryAssociation' {ipamResourceDiscoveryId} -> ipamResourceDiscoveryId) (\s@IpamResourceDiscoveryAssociation' {} a -> s {ipamResourceDiscoveryId = a} :: IpamResourceDiscoveryAssociation)

-- | Defines if the resource discovery is the default. When you create an
-- IPAM, a default resource discovery is created for your IPAM and it\'s
-- associated with your IPAM.
ipamResourceDiscoveryAssociation_isDefault :: Lens.Lens' IpamResourceDiscoveryAssociation (Prelude.Maybe Prelude.Bool)
ipamResourceDiscoveryAssociation_isDefault = Lens.lens (\IpamResourceDiscoveryAssociation' {isDefault} -> isDefault) (\s@IpamResourceDiscoveryAssociation' {} a -> s {isDefault = a} :: IpamResourceDiscoveryAssociation)

-- | The Amazon Web Services account ID of the resource discovery owner.
ipamResourceDiscoveryAssociation_ownerId :: Lens.Lens' IpamResourceDiscoveryAssociation (Prelude.Maybe Prelude.Text)
ipamResourceDiscoveryAssociation_ownerId = Lens.lens (\IpamResourceDiscoveryAssociation' {ownerId} -> ownerId) (\s@IpamResourceDiscoveryAssociation' {} a -> s {ownerId = a} :: IpamResourceDiscoveryAssociation)

-- | The resource discovery status.
--
-- -   @active@ - Connection or permissions required to read the results of
--     the resource discovery are intact.
--
-- -   @not-found@ - Connection or permissions required to read the results
--     of the resource discovery are broken. This may happen if the owner
--     of the resource discovery stopped sharing it or deleted the resource
--     discovery. Verify the resource discovery still exists and the Amazon
--     Web Services RAM resource share is still intact.
ipamResourceDiscoveryAssociation_resourceDiscoveryStatus :: Lens.Lens' IpamResourceDiscoveryAssociation (Prelude.Maybe IpamAssociatedResourceDiscoveryStatus)
ipamResourceDiscoveryAssociation_resourceDiscoveryStatus = Lens.lens (\IpamResourceDiscoveryAssociation' {resourceDiscoveryStatus} -> resourceDiscoveryStatus) (\s@IpamResourceDiscoveryAssociation' {} a -> s {resourceDiscoveryStatus = a} :: IpamResourceDiscoveryAssociation)

-- | The lifecycle state of the association when you associate or
-- disassociate a resource discovery.
--
-- -   @associate-in-progress@ - Resource discovery is being associated.
--
-- -   @associate-complete@ - Resource discovery association is complete.
--
-- -   @associate-failed@ - Resource discovery association has failed.
--
-- -   @disassociate-in-progress@ - Resource discovery is being
--     disassociated.
--
-- -   @disassociate-complete@ - Resource discovery disassociation is
--     complete.
--
-- -   @disassociate-failed @ - Resource discovery disassociation has
--     failed.
--
-- -   @isolate-in-progress@ - Amazon Web Services account that created the
--     resource discovery association has been removed and the resource
--     discovery associatation is being isolated.
--
-- -   @isolate-complete@ - Resource discovery isolation is complete..
--
-- -   @restore-in-progress@ - Resource discovery is being restored.
ipamResourceDiscoveryAssociation_state :: Lens.Lens' IpamResourceDiscoveryAssociation (Prelude.Maybe IpamResourceDiscoveryAssociationState)
ipamResourceDiscoveryAssociation_state = Lens.lens (\IpamResourceDiscoveryAssociation' {state} -> state) (\s@IpamResourceDiscoveryAssociation' {} a -> s {state = a} :: IpamResourceDiscoveryAssociation)

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
ipamResourceDiscoveryAssociation_tags :: Lens.Lens' IpamResourceDiscoveryAssociation (Prelude.Maybe [Tag])
ipamResourceDiscoveryAssociation_tags = Lens.lens (\IpamResourceDiscoveryAssociation' {tags} -> tags) (\s@IpamResourceDiscoveryAssociation' {} a -> s {tags = a} :: IpamResourceDiscoveryAssociation) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromXML
    IpamResourceDiscoveryAssociation
  where
  parseXML x =
    IpamResourceDiscoveryAssociation'
      Prelude.<$> (x Data..@? "ipamArn")
      Prelude.<*> (x Data..@? "ipamId")
      Prelude.<*> (x Data..@? "ipamRegion")
      Prelude.<*> (x Data..@? "ipamResourceDiscoveryAssociationArn")
      Prelude.<*> (x Data..@? "ipamResourceDiscoveryAssociationId")
      Prelude.<*> (x Data..@? "ipamResourceDiscoveryId")
      Prelude.<*> (x Data..@? "isDefault")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "resourceDiscoveryStatus")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance
  Prelude.Hashable
    IpamResourceDiscoveryAssociation
  where
  hashWithSalt
    _salt
    IpamResourceDiscoveryAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` ipamArn
        `Prelude.hashWithSalt` ipamId
        `Prelude.hashWithSalt` ipamRegion
        `Prelude.hashWithSalt` ipamResourceDiscoveryAssociationArn
        `Prelude.hashWithSalt` ipamResourceDiscoveryAssociationId
        `Prelude.hashWithSalt` ipamResourceDiscoveryId
        `Prelude.hashWithSalt` isDefault
        `Prelude.hashWithSalt` ownerId
        `Prelude.hashWithSalt` resourceDiscoveryStatus
        `Prelude.hashWithSalt` state
        `Prelude.hashWithSalt` tags

instance
  Prelude.NFData
    IpamResourceDiscoveryAssociation
  where
  rnf IpamResourceDiscoveryAssociation' {..} =
    Prelude.rnf ipamArn
      `Prelude.seq` Prelude.rnf ipamId
      `Prelude.seq` Prelude.rnf ipamRegion
      `Prelude.seq` Prelude.rnf ipamResourceDiscoveryAssociationArn
      `Prelude.seq` Prelude.rnf ipamResourceDiscoveryAssociationId
      `Prelude.seq` Prelude.rnf ipamResourceDiscoveryId
      `Prelude.seq` Prelude.rnf isDefault
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf resourceDiscoveryStatus
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
