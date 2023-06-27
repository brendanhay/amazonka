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
-- Module      : Amazonka.EC2.Types.IpamResourceDiscovery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamResourceDiscovery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.IpamOperatingRegion
import Amazonka.EC2.Types.IpamResourceDiscoveryState
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | A resource discovery is an IPAM component that enables IPAM to manage
-- and monitor resources that belong to the owning account.
--
-- /See:/ 'newIpamResourceDiscovery' smart constructor.
data IpamResourceDiscovery = IpamResourceDiscovery'
  { -- | The resource discovery description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The resource discovery Amazon Resource Name (ARN).
    ipamResourceDiscoveryArn :: Prelude.Maybe Prelude.Text,
    -- | The resource discovery ID.
    ipamResourceDiscoveryId :: Prelude.Maybe Prelude.Text,
    -- | The resource discovery Region.
    ipamResourceDiscoveryRegion :: Prelude.Maybe Prelude.Text,
    -- | Defines if the resource discovery is the default. The default resource
    -- discovery is the resource discovery automatically created when you
    -- create an IPAM.
    isDefault :: Prelude.Maybe Prelude.Bool,
    -- | The operating Regions for the resource discovery. Operating Regions are
    -- Amazon Web Services Regions where the IPAM is allowed to manage IP
    -- address CIDRs. IPAM only discovers and monitors resources in the Amazon
    -- Web Services Regions you select as operating Regions.
    operatingRegions :: Prelude.Maybe [IpamOperatingRegion],
    -- | The ID of the owner.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The lifecycle state of the resource discovery.
    --
    -- -   @create-in-progress@ - Resource discovery is being created.
    --
    -- -   @create-complete@ - Resource discovery creation is complete.
    --
    -- -   @create-failed@ - Resource discovery creation has failed.
    --
    -- -   @modify-in-progress@ - Resource discovery is being modified.
    --
    -- -   @modify-complete@ - Resource discovery modification is complete.
    --
    -- -   @modify-failed@ - Resource discovery modification has failed.
    --
    -- -   @delete-in-progress@ - Resource discovery is being deleted.
    --
    -- -   @delete-complete@ - Resource discovery deletion is complete.
    --
    -- -   @delete-failed@ - Resource discovery deletion has failed.
    --
    -- -   @isolate-in-progress@ - Amazon Web Services account that created the
    --     resource discovery has been removed and the resource discovery is
    --     being isolated.
    --
    -- -   @isolate-complete@ - Resource discovery isolation is complete.
    --
    -- -   @restore-in-progress@ - Amazon Web Services account that created the
    --     resource discovery and was isolated has been restored.
    state :: Prelude.Maybe IpamResourceDiscoveryState,
    -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value. You can use tags to
    -- search and filter your resources or track your Amazon Web Services
    -- costs.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpamResourceDiscovery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'ipamResourceDiscovery_description' - The resource discovery description.
--
-- 'ipamResourceDiscoveryArn', 'ipamResourceDiscovery_ipamResourceDiscoveryArn' - The resource discovery Amazon Resource Name (ARN).
--
-- 'ipamResourceDiscoveryId', 'ipamResourceDiscovery_ipamResourceDiscoveryId' - The resource discovery ID.
--
-- 'ipamResourceDiscoveryRegion', 'ipamResourceDiscovery_ipamResourceDiscoveryRegion' - The resource discovery Region.
--
-- 'isDefault', 'ipamResourceDiscovery_isDefault' - Defines if the resource discovery is the default. The default resource
-- discovery is the resource discovery automatically created when you
-- create an IPAM.
--
-- 'operatingRegions', 'ipamResourceDiscovery_operatingRegions' - The operating Regions for the resource discovery. Operating Regions are
-- Amazon Web Services Regions where the IPAM is allowed to manage IP
-- address CIDRs. IPAM only discovers and monitors resources in the Amazon
-- Web Services Regions you select as operating Regions.
--
-- 'ownerId', 'ipamResourceDiscovery_ownerId' - The ID of the owner.
--
-- 'state', 'ipamResourceDiscovery_state' - The lifecycle state of the resource discovery.
--
-- -   @create-in-progress@ - Resource discovery is being created.
--
-- -   @create-complete@ - Resource discovery creation is complete.
--
-- -   @create-failed@ - Resource discovery creation has failed.
--
-- -   @modify-in-progress@ - Resource discovery is being modified.
--
-- -   @modify-complete@ - Resource discovery modification is complete.
--
-- -   @modify-failed@ - Resource discovery modification has failed.
--
-- -   @delete-in-progress@ - Resource discovery is being deleted.
--
-- -   @delete-complete@ - Resource discovery deletion is complete.
--
-- -   @delete-failed@ - Resource discovery deletion has failed.
--
-- -   @isolate-in-progress@ - Amazon Web Services account that created the
--     resource discovery has been removed and the resource discovery is
--     being isolated.
--
-- -   @isolate-complete@ - Resource discovery isolation is complete.
--
-- -   @restore-in-progress@ - Amazon Web Services account that created the
--     resource discovery and was isolated has been restored.
--
-- 'tags', 'ipamResourceDiscovery_tags' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
newIpamResourceDiscovery ::
  IpamResourceDiscovery
newIpamResourceDiscovery =
  IpamResourceDiscovery'
    { description =
        Prelude.Nothing,
      ipamResourceDiscoveryArn = Prelude.Nothing,
      ipamResourceDiscoveryId = Prelude.Nothing,
      ipamResourceDiscoveryRegion = Prelude.Nothing,
      isDefault = Prelude.Nothing,
      operatingRegions = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The resource discovery description.
ipamResourceDiscovery_description :: Lens.Lens' IpamResourceDiscovery (Prelude.Maybe Prelude.Text)
ipamResourceDiscovery_description = Lens.lens (\IpamResourceDiscovery' {description} -> description) (\s@IpamResourceDiscovery' {} a -> s {description = a} :: IpamResourceDiscovery)

-- | The resource discovery Amazon Resource Name (ARN).
ipamResourceDiscovery_ipamResourceDiscoveryArn :: Lens.Lens' IpamResourceDiscovery (Prelude.Maybe Prelude.Text)
ipamResourceDiscovery_ipamResourceDiscoveryArn = Lens.lens (\IpamResourceDiscovery' {ipamResourceDiscoveryArn} -> ipamResourceDiscoveryArn) (\s@IpamResourceDiscovery' {} a -> s {ipamResourceDiscoveryArn = a} :: IpamResourceDiscovery)

-- | The resource discovery ID.
ipamResourceDiscovery_ipamResourceDiscoveryId :: Lens.Lens' IpamResourceDiscovery (Prelude.Maybe Prelude.Text)
ipamResourceDiscovery_ipamResourceDiscoveryId = Lens.lens (\IpamResourceDiscovery' {ipamResourceDiscoveryId} -> ipamResourceDiscoveryId) (\s@IpamResourceDiscovery' {} a -> s {ipamResourceDiscoveryId = a} :: IpamResourceDiscovery)

-- | The resource discovery Region.
ipamResourceDiscovery_ipamResourceDiscoveryRegion :: Lens.Lens' IpamResourceDiscovery (Prelude.Maybe Prelude.Text)
ipamResourceDiscovery_ipamResourceDiscoveryRegion = Lens.lens (\IpamResourceDiscovery' {ipamResourceDiscoveryRegion} -> ipamResourceDiscoveryRegion) (\s@IpamResourceDiscovery' {} a -> s {ipamResourceDiscoveryRegion = a} :: IpamResourceDiscovery)

-- | Defines if the resource discovery is the default. The default resource
-- discovery is the resource discovery automatically created when you
-- create an IPAM.
ipamResourceDiscovery_isDefault :: Lens.Lens' IpamResourceDiscovery (Prelude.Maybe Prelude.Bool)
ipamResourceDiscovery_isDefault = Lens.lens (\IpamResourceDiscovery' {isDefault} -> isDefault) (\s@IpamResourceDiscovery' {} a -> s {isDefault = a} :: IpamResourceDiscovery)

-- | The operating Regions for the resource discovery. Operating Regions are
-- Amazon Web Services Regions where the IPAM is allowed to manage IP
-- address CIDRs. IPAM only discovers and monitors resources in the Amazon
-- Web Services Regions you select as operating Regions.
ipamResourceDiscovery_operatingRegions :: Lens.Lens' IpamResourceDiscovery (Prelude.Maybe [IpamOperatingRegion])
ipamResourceDiscovery_operatingRegions = Lens.lens (\IpamResourceDiscovery' {operatingRegions} -> operatingRegions) (\s@IpamResourceDiscovery' {} a -> s {operatingRegions = a} :: IpamResourceDiscovery) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the owner.
ipamResourceDiscovery_ownerId :: Lens.Lens' IpamResourceDiscovery (Prelude.Maybe Prelude.Text)
ipamResourceDiscovery_ownerId = Lens.lens (\IpamResourceDiscovery' {ownerId} -> ownerId) (\s@IpamResourceDiscovery' {} a -> s {ownerId = a} :: IpamResourceDiscovery)

-- | The lifecycle state of the resource discovery.
--
-- -   @create-in-progress@ - Resource discovery is being created.
--
-- -   @create-complete@ - Resource discovery creation is complete.
--
-- -   @create-failed@ - Resource discovery creation has failed.
--
-- -   @modify-in-progress@ - Resource discovery is being modified.
--
-- -   @modify-complete@ - Resource discovery modification is complete.
--
-- -   @modify-failed@ - Resource discovery modification has failed.
--
-- -   @delete-in-progress@ - Resource discovery is being deleted.
--
-- -   @delete-complete@ - Resource discovery deletion is complete.
--
-- -   @delete-failed@ - Resource discovery deletion has failed.
--
-- -   @isolate-in-progress@ - Amazon Web Services account that created the
--     resource discovery has been removed and the resource discovery is
--     being isolated.
--
-- -   @isolate-complete@ - Resource discovery isolation is complete.
--
-- -   @restore-in-progress@ - Amazon Web Services account that created the
--     resource discovery and was isolated has been restored.
ipamResourceDiscovery_state :: Lens.Lens' IpamResourceDiscovery (Prelude.Maybe IpamResourceDiscoveryState)
ipamResourceDiscovery_state = Lens.lens (\IpamResourceDiscovery' {state} -> state) (\s@IpamResourceDiscovery' {} a -> s {state = a} :: IpamResourceDiscovery)

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
ipamResourceDiscovery_tags :: Lens.Lens' IpamResourceDiscovery (Prelude.Maybe [Tag])
ipamResourceDiscovery_tags = Lens.lens (\IpamResourceDiscovery' {tags} -> tags) (\s@IpamResourceDiscovery' {} a -> s {tags = a} :: IpamResourceDiscovery) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML IpamResourceDiscovery where
  parseXML x =
    IpamResourceDiscovery'
      Prelude.<$> (x Data..@? "description")
      Prelude.<*> (x Data..@? "ipamResourceDiscoveryArn")
      Prelude.<*> (x Data..@? "ipamResourceDiscoveryId")
      Prelude.<*> (x Data..@? "ipamResourceDiscoveryRegion")
      Prelude.<*> (x Data..@? "isDefault")
      Prelude.<*> ( x
                      Data..@? "operatingRegionSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable IpamResourceDiscovery where
  hashWithSalt _salt IpamResourceDiscovery' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` ipamResourceDiscoveryArn
      `Prelude.hashWithSalt` ipamResourceDiscoveryId
      `Prelude.hashWithSalt` ipamResourceDiscoveryRegion
      `Prelude.hashWithSalt` isDefault
      `Prelude.hashWithSalt` operatingRegions
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags

instance Prelude.NFData IpamResourceDiscovery where
  rnf IpamResourceDiscovery' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf ipamResourceDiscoveryArn
      `Prelude.seq` Prelude.rnf ipamResourceDiscoveryId
      `Prelude.seq` Prelude.rnf ipamResourceDiscoveryRegion
      `Prelude.seq` Prelude.rnf isDefault
      `Prelude.seq` Prelude.rnf operatingRegions
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
