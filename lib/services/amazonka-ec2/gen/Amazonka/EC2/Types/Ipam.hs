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
-- Module      : Amazonka.EC2.Types.Ipam
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Ipam where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.IpamOperatingRegion
import Amazonka.EC2.Types.IpamState
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | IPAM is a VPC feature that you can use to automate your IP address
-- management workflows including assigning, tracking, troubleshooting, and
-- auditing IP addresses across Amazon Web Services Regions and accounts
-- throughout your Amazon Web Services Organization. For more information,
-- see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
-- in the /Amazon VPC IPAM User Guide/.
--
-- /See:/ 'newIpam' smart constructor.
data Ipam = Ipam'
  { -- | The description for the IPAM.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IPAM.
    ipamArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the IPAM.
    ipamId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region of the IPAM.
    ipamRegion :: Prelude.Maybe Prelude.Text,
    -- | The operating Regions for an IPAM. Operating Regions are Amazon Web
    -- Services Regions where the IPAM is allowed to manage IP address CIDRs.
    -- IPAM only discovers and monitors resources in the Amazon Web Services
    -- Regions you select as operating Regions.
    --
    -- For more information about operating Regions, see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/create-ipam.html Create an IPAM>
    -- in the /Amazon VPC IPAM User Guide/.
    operatingRegions :: Prelude.Maybe [IpamOperatingRegion],
    -- | The Amazon Web Services account ID of the owner of the IPAM.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the IPAM\'s default private scope.
    privateDefaultScopeId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the IPAM\'s default public scope.
    publicDefaultScopeId :: Prelude.Maybe Prelude.Text,
    -- | The number of scopes in the IPAM. The scope quota is 5. For more
    -- information on quotas, see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/quotas-ipam.html Quotas in IPAM>
    -- in the /Amazon VPC IPAM User Guide/.
    scopeCount :: Prelude.Maybe Prelude.Int,
    -- | The state of the IPAM.
    state :: Prelude.Maybe IpamState,
    -- | The key\/value combination of a tag assigned to the resource. Use the
    -- tag key in the filter name and the tag value as the filter value. For
    -- example, to find all resources that have a tag with the key @Owner@ and
    -- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
    -- for the filter value.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ipam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'ipam_description' - The description for the IPAM.
--
-- 'ipamArn', 'ipam_ipamArn' - The ARN of the IPAM.
--
-- 'ipamId', 'ipam_ipamId' - The ID of the IPAM.
--
-- 'ipamRegion', 'ipam_ipamRegion' - The Amazon Web Services Region of the IPAM.
--
-- 'operatingRegions', 'ipam_operatingRegions' - The operating Regions for an IPAM. Operating Regions are Amazon Web
-- Services Regions where the IPAM is allowed to manage IP address CIDRs.
-- IPAM only discovers and monitors resources in the Amazon Web Services
-- Regions you select as operating Regions.
--
-- For more information about operating Regions, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/create-ipam.html Create an IPAM>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'ownerId', 'ipam_ownerId' - The Amazon Web Services account ID of the owner of the IPAM.
--
-- 'privateDefaultScopeId', 'ipam_privateDefaultScopeId' - The ID of the IPAM\'s default private scope.
--
-- 'publicDefaultScopeId', 'ipam_publicDefaultScopeId' - The ID of the IPAM\'s default public scope.
--
-- 'scopeCount', 'ipam_scopeCount' - The number of scopes in the IPAM. The scope quota is 5. For more
-- information on quotas, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/quotas-ipam.html Quotas in IPAM>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'state', 'ipam_state' - The state of the IPAM.
--
-- 'tags', 'ipam_tags' - The key\/value combination of a tag assigned to the resource. Use the
-- tag key in the filter name and the tag value as the filter value. For
-- example, to find all resources that have a tag with the key @Owner@ and
-- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
-- for the filter value.
newIpam ::
  Ipam
newIpam =
  Ipam'
    { description = Prelude.Nothing,
      ipamArn = Prelude.Nothing,
      ipamId = Prelude.Nothing,
      ipamRegion = Prelude.Nothing,
      operatingRegions = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      privateDefaultScopeId = Prelude.Nothing,
      publicDefaultScopeId = Prelude.Nothing,
      scopeCount = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The description for the IPAM.
ipam_description :: Lens.Lens' Ipam (Prelude.Maybe Prelude.Text)
ipam_description = Lens.lens (\Ipam' {description} -> description) (\s@Ipam' {} a -> s {description = a} :: Ipam)

-- | The ARN of the IPAM.
ipam_ipamArn :: Lens.Lens' Ipam (Prelude.Maybe Prelude.Text)
ipam_ipamArn = Lens.lens (\Ipam' {ipamArn} -> ipamArn) (\s@Ipam' {} a -> s {ipamArn = a} :: Ipam)

-- | The ID of the IPAM.
ipam_ipamId :: Lens.Lens' Ipam (Prelude.Maybe Prelude.Text)
ipam_ipamId = Lens.lens (\Ipam' {ipamId} -> ipamId) (\s@Ipam' {} a -> s {ipamId = a} :: Ipam)

-- | The Amazon Web Services Region of the IPAM.
ipam_ipamRegion :: Lens.Lens' Ipam (Prelude.Maybe Prelude.Text)
ipam_ipamRegion = Lens.lens (\Ipam' {ipamRegion} -> ipamRegion) (\s@Ipam' {} a -> s {ipamRegion = a} :: Ipam)

-- | The operating Regions for an IPAM. Operating Regions are Amazon Web
-- Services Regions where the IPAM is allowed to manage IP address CIDRs.
-- IPAM only discovers and monitors resources in the Amazon Web Services
-- Regions you select as operating Regions.
--
-- For more information about operating Regions, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/create-ipam.html Create an IPAM>
-- in the /Amazon VPC IPAM User Guide/.
ipam_operatingRegions :: Lens.Lens' Ipam (Prelude.Maybe [IpamOperatingRegion])
ipam_operatingRegions = Lens.lens (\Ipam' {operatingRegions} -> operatingRegions) (\s@Ipam' {} a -> s {operatingRegions = a} :: Ipam) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account ID of the owner of the IPAM.
ipam_ownerId :: Lens.Lens' Ipam (Prelude.Maybe Prelude.Text)
ipam_ownerId = Lens.lens (\Ipam' {ownerId} -> ownerId) (\s@Ipam' {} a -> s {ownerId = a} :: Ipam)

-- | The ID of the IPAM\'s default private scope.
ipam_privateDefaultScopeId :: Lens.Lens' Ipam (Prelude.Maybe Prelude.Text)
ipam_privateDefaultScopeId = Lens.lens (\Ipam' {privateDefaultScopeId} -> privateDefaultScopeId) (\s@Ipam' {} a -> s {privateDefaultScopeId = a} :: Ipam)

-- | The ID of the IPAM\'s default public scope.
ipam_publicDefaultScopeId :: Lens.Lens' Ipam (Prelude.Maybe Prelude.Text)
ipam_publicDefaultScopeId = Lens.lens (\Ipam' {publicDefaultScopeId} -> publicDefaultScopeId) (\s@Ipam' {} a -> s {publicDefaultScopeId = a} :: Ipam)

-- | The number of scopes in the IPAM. The scope quota is 5. For more
-- information on quotas, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/quotas-ipam.html Quotas in IPAM>
-- in the /Amazon VPC IPAM User Guide/.
ipam_scopeCount :: Lens.Lens' Ipam (Prelude.Maybe Prelude.Int)
ipam_scopeCount = Lens.lens (\Ipam' {scopeCount} -> scopeCount) (\s@Ipam' {} a -> s {scopeCount = a} :: Ipam)

-- | The state of the IPAM.
ipam_state :: Lens.Lens' Ipam (Prelude.Maybe IpamState)
ipam_state = Lens.lens (\Ipam' {state} -> state) (\s@Ipam' {} a -> s {state = a} :: Ipam)

-- | The key\/value combination of a tag assigned to the resource. Use the
-- tag key in the filter name and the tag value as the filter value. For
-- example, to find all resources that have a tag with the key @Owner@ and
-- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
-- for the filter value.
ipam_tags :: Lens.Lens' Ipam (Prelude.Maybe [Tag])
ipam_tags = Lens.lens (\Ipam' {tags} -> tags) (\s@Ipam' {} a -> s {tags = a} :: Ipam) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML Ipam where
  parseXML x =
    Ipam'
      Prelude.<$> (x Data..@? "description")
      Prelude.<*> (x Data..@? "ipamArn")
      Prelude.<*> (x Data..@? "ipamId")
      Prelude.<*> (x Data..@? "ipamRegion")
      Prelude.<*> ( x
                      Data..@? "operatingRegionSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "privateDefaultScopeId")
      Prelude.<*> (x Data..@? "publicDefaultScopeId")
      Prelude.<*> (x Data..@? "scopeCount")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable Ipam where
  hashWithSalt _salt Ipam' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` ipamArn
      `Prelude.hashWithSalt` ipamId
      `Prelude.hashWithSalt` ipamRegion
      `Prelude.hashWithSalt` operatingRegions
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` privateDefaultScopeId
      `Prelude.hashWithSalt` publicDefaultScopeId
      `Prelude.hashWithSalt` scopeCount
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Ipam where
  rnf Ipam' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf ipamArn `Prelude.seq`
        Prelude.rnf ipamId `Prelude.seq`
          Prelude.rnf ipamRegion `Prelude.seq`
            Prelude.rnf operatingRegions `Prelude.seq`
              Prelude.rnf ownerId `Prelude.seq`
                Prelude.rnf privateDefaultScopeId `Prelude.seq`
                  Prelude.rnf publicDefaultScopeId `Prelude.seq`
                    Prelude.rnf scopeCount `Prelude.seq`
                      Prelude.rnf state `Prelude.seq`
                        Prelude.rnf tags
