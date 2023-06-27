{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.CreateIpamResourceDiscovery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an IPAM resource discovery. A resource discovery is an IPAM
-- component that enables IPAM to manage and monitor resources that belong
-- to the owning account.
module Amazonka.EC2.CreateIpamResourceDiscovery
  ( -- * Creating a Request
    CreateIpamResourceDiscovery (..),
    newCreateIpamResourceDiscovery,

    -- * Request Lenses
    createIpamResourceDiscovery_clientToken,
    createIpamResourceDiscovery_description,
    createIpamResourceDiscovery_dryRun,
    createIpamResourceDiscovery_operatingRegions,
    createIpamResourceDiscovery_tagSpecifications,

    -- * Destructuring the Response
    CreateIpamResourceDiscoveryResponse (..),
    newCreateIpamResourceDiscoveryResponse,

    -- * Response Lenses
    createIpamResourceDiscoveryResponse_ipamResourceDiscovery,
    createIpamResourceDiscoveryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateIpamResourceDiscovery' smart constructor.
data CreateIpamResourceDiscovery = CreateIpamResourceDiscovery'
  { -- | A client token for the IPAM resource discovery.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the IPAM resource discovery.
    description :: Prelude.Maybe Prelude.Text,
    -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Operating Regions for the IPAM resource discovery. Operating Regions are
    -- Amazon Web Services Regions where the IPAM is allowed to manage IP
    -- address CIDRs. IPAM only discovers and monitors resources in the Amazon
    -- Web Services Regions you select as operating Regions.
    operatingRegions :: Prelude.Maybe [AddIpamOperatingRegion],
    -- | Tag specifications for the IPAM resource discovery.
    tagSpecifications :: Prelude.Maybe [TagSpecification]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIpamResourceDiscovery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createIpamResourceDiscovery_clientToken' - A client token for the IPAM resource discovery.
--
-- 'description', 'createIpamResourceDiscovery_description' - A description for the IPAM resource discovery.
--
-- 'dryRun', 'createIpamResourceDiscovery_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'operatingRegions', 'createIpamResourceDiscovery_operatingRegions' - Operating Regions for the IPAM resource discovery. Operating Regions are
-- Amazon Web Services Regions where the IPAM is allowed to manage IP
-- address CIDRs. IPAM only discovers and monitors resources in the Amazon
-- Web Services Regions you select as operating Regions.
--
-- 'tagSpecifications', 'createIpamResourceDiscovery_tagSpecifications' - Tag specifications for the IPAM resource discovery.
newCreateIpamResourceDiscovery ::
  CreateIpamResourceDiscovery
newCreateIpamResourceDiscovery =
  CreateIpamResourceDiscovery'
    { clientToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      operatingRegions = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing
    }

-- | A client token for the IPAM resource discovery.
createIpamResourceDiscovery_clientToken :: Lens.Lens' CreateIpamResourceDiscovery (Prelude.Maybe Prelude.Text)
createIpamResourceDiscovery_clientToken = Lens.lens (\CreateIpamResourceDiscovery' {clientToken} -> clientToken) (\s@CreateIpamResourceDiscovery' {} a -> s {clientToken = a} :: CreateIpamResourceDiscovery)

-- | A description for the IPAM resource discovery.
createIpamResourceDiscovery_description :: Lens.Lens' CreateIpamResourceDiscovery (Prelude.Maybe Prelude.Text)
createIpamResourceDiscovery_description = Lens.lens (\CreateIpamResourceDiscovery' {description} -> description) (\s@CreateIpamResourceDiscovery' {} a -> s {description = a} :: CreateIpamResourceDiscovery)

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
createIpamResourceDiscovery_dryRun :: Lens.Lens' CreateIpamResourceDiscovery (Prelude.Maybe Prelude.Bool)
createIpamResourceDiscovery_dryRun = Lens.lens (\CreateIpamResourceDiscovery' {dryRun} -> dryRun) (\s@CreateIpamResourceDiscovery' {} a -> s {dryRun = a} :: CreateIpamResourceDiscovery)

-- | Operating Regions for the IPAM resource discovery. Operating Regions are
-- Amazon Web Services Regions where the IPAM is allowed to manage IP
-- address CIDRs. IPAM only discovers and monitors resources in the Amazon
-- Web Services Regions you select as operating Regions.
createIpamResourceDiscovery_operatingRegions :: Lens.Lens' CreateIpamResourceDiscovery (Prelude.Maybe [AddIpamOperatingRegion])
createIpamResourceDiscovery_operatingRegions = Lens.lens (\CreateIpamResourceDiscovery' {operatingRegions} -> operatingRegions) (\s@CreateIpamResourceDiscovery' {} a -> s {operatingRegions = a} :: CreateIpamResourceDiscovery) Prelude.. Lens.mapping Lens.coerced

-- | Tag specifications for the IPAM resource discovery.
createIpamResourceDiscovery_tagSpecifications :: Lens.Lens' CreateIpamResourceDiscovery (Prelude.Maybe [TagSpecification])
createIpamResourceDiscovery_tagSpecifications = Lens.lens (\CreateIpamResourceDiscovery' {tagSpecifications} -> tagSpecifications) (\s@CreateIpamResourceDiscovery' {} a -> s {tagSpecifications = a} :: CreateIpamResourceDiscovery) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateIpamResourceDiscovery where
  type
    AWSResponse CreateIpamResourceDiscovery =
      CreateIpamResourceDiscoveryResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateIpamResourceDiscoveryResponse'
            Prelude.<$> (x Data..@? "ipamResourceDiscovery")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateIpamResourceDiscovery where
  hashWithSalt _salt CreateIpamResourceDiscovery' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` operatingRegions
      `Prelude.hashWithSalt` tagSpecifications

instance Prelude.NFData CreateIpamResourceDiscovery where
  rnf CreateIpamResourceDiscovery' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf operatingRegions
      `Prelude.seq` Prelude.rnf tagSpecifications

instance Data.ToHeaders CreateIpamResourceDiscovery where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateIpamResourceDiscovery where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateIpamResourceDiscovery where
  toQuery CreateIpamResourceDiscovery' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateIpamResourceDiscovery" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "OperatingRegion"
              Prelude.<$> operatingRegions
          ),
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          )
      ]

-- | /See:/ 'newCreateIpamResourceDiscoveryResponse' smart constructor.
data CreateIpamResourceDiscoveryResponse = CreateIpamResourceDiscoveryResponse'
  { -- | An IPAM resource discovery.
    ipamResourceDiscovery :: Prelude.Maybe IpamResourceDiscovery,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIpamResourceDiscoveryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamResourceDiscovery', 'createIpamResourceDiscoveryResponse_ipamResourceDiscovery' - An IPAM resource discovery.
--
-- 'httpStatus', 'createIpamResourceDiscoveryResponse_httpStatus' - The response's http status code.
newCreateIpamResourceDiscoveryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateIpamResourceDiscoveryResponse
newCreateIpamResourceDiscoveryResponse pHttpStatus_ =
  CreateIpamResourceDiscoveryResponse'
    { ipamResourceDiscovery =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An IPAM resource discovery.
createIpamResourceDiscoveryResponse_ipamResourceDiscovery :: Lens.Lens' CreateIpamResourceDiscoveryResponse (Prelude.Maybe IpamResourceDiscovery)
createIpamResourceDiscoveryResponse_ipamResourceDiscovery = Lens.lens (\CreateIpamResourceDiscoveryResponse' {ipamResourceDiscovery} -> ipamResourceDiscovery) (\s@CreateIpamResourceDiscoveryResponse' {} a -> s {ipamResourceDiscovery = a} :: CreateIpamResourceDiscoveryResponse)

-- | The response's http status code.
createIpamResourceDiscoveryResponse_httpStatus :: Lens.Lens' CreateIpamResourceDiscoveryResponse Prelude.Int
createIpamResourceDiscoveryResponse_httpStatus = Lens.lens (\CreateIpamResourceDiscoveryResponse' {httpStatus} -> httpStatus) (\s@CreateIpamResourceDiscoveryResponse' {} a -> s {httpStatus = a} :: CreateIpamResourceDiscoveryResponse)

instance
  Prelude.NFData
    CreateIpamResourceDiscoveryResponse
  where
  rnf CreateIpamResourceDiscoveryResponse' {..} =
    Prelude.rnf ipamResourceDiscovery
      `Prelude.seq` Prelude.rnf httpStatus
