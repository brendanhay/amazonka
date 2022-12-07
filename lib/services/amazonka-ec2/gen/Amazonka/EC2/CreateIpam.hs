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
-- Module      : Amazonka.EC2.CreateIpam
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an IPAM. Amazon VPC IP Address Manager (IPAM) is a VPC feature
-- that you can use to automate your IP address management workflows
-- including assigning, tracking, troubleshooting, and auditing IP
-- addresses across Amazon Web Services Regions and accounts throughout
-- your Amazon Web Services Organization.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/create-ipam.html Create an IPAM>
-- in the /Amazon VPC IPAM User Guide/.
module Amazonka.EC2.CreateIpam
  ( -- * Creating a Request
    CreateIpam (..),
    newCreateIpam,

    -- * Request Lenses
    createIpam_clientToken,
    createIpam_description,
    createIpam_dryRun,
    createIpam_operatingRegions,
    createIpam_tagSpecifications,

    -- * Destructuring the Response
    CreateIpamResponse (..),
    newCreateIpamResponse,

    -- * Response Lenses
    createIpamResponse_ipam,
    createIpamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateIpam' smart constructor.
data CreateIpam = CreateIpam'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the IPAM.
    description :: Prelude.Maybe Prelude.Text,
    -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The operating Regions for the IPAM. Operating Regions are Amazon Web
    -- Services Regions where the IPAM is allowed to manage IP address CIDRs.
    -- IPAM only discovers and monitors resources in the Amazon Web Services
    -- Regions you select as operating Regions.
    --
    -- For more information about operating Regions, see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/create-ipam.html Create an IPAM>
    -- in the /Amazon VPC IPAM User Guide/.
    operatingRegions :: Prelude.Maybe [AddIpamOperatingRegion],
    -- | The key\/value combination of a tag assigned to the resource. Use the
    -- tag key in the filter name and the tag value as the filter value. For
    -- example, to find all resources that have a tag with the key @Owner@ and
    -- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
    -- for the filter value.
    tagSpecifications :: Prelude.Maybe [TagSpecification]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIpam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createIpam_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'description', 'createIpam_description' - A description for the IPAM.
--
-- 'dryRun', 'createIpam_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'operatingRegions', 'createIpam_operatingRegions' - The operating Regions for the IPAM. Operating Regions are Amazon Web
-- Services Regions where the IPAM is allowed to manage IP address CIDRs.
-- IPAM only discovers and monitors resources in the Amazon Web Services
-- Regions you select as operating Regions.
--
-- For more information about operating Regions, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/create-ipam.html Create an IPAM>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'tagSpecifications', 'createIpam_tagSpecifications' - The key\/value combination of a tag assigned to the resource. Use the
-- tag key in the filter name and the tag value as the filter value. For
-- example, to find all resources that have a tag with the key @Owner@ and
-- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
-- for the filter value.
newCreateIpam ::
  CreateIpam
newCreateIpam =
  CreateIpam'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      operatingRegions = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
createIpam_clientToken :: Lens.Lens' CreateIpam (Prelude.Maybe Prelude.Text)
createIpam_clientToken = Lens.lens (\CreateIpam' {clientToken} -> clientToken) (\s@CreateIpam' {} a -> s {clientToken = a} :: CreateIpam)

-- | A description for the IPAM.
createIpam_description :: Lens.Lens' CreateIpam (Prelude.Maybe Prelude.Text)
createIpam_description = Lens.lens (\CreateIpam' {description} -> description) (\s@CreateIpam' {} a -> s {description = a} :: CreateIpam)

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
createIpam_dryRun :: Lens.Lens' CreateIpam (Prelude.Maybe Prelude.Bool)
createIpam_dryRun = Lens.lens (\CreateIpam' {dryRun} -> dryRun) (\s@CreateIpam' {} a -> s {dryRun = a} :: CreateIpam)

-- | The operating Regions for the IPAM. Operating Regions are Amazon Web
-- Services Regions where the IPAM is allowed to manage IP address CIDRs.
-- IPAM only discovers and monitors resources in the Amazon Web Services
-- Regions you select as operating Regions.
--
-- For more information about operating Regions, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/create-ipam.html Create an IPAM>
-- in the /Amazon VPC IPAM User Guide/.
createIpam_operatingRegions :: Lens.Lens' CreateIpam (Prelude.Maybe [AddIpamOperatingRegion])
createIpam_operatingRegions = Lens.lens (\CreateIpam' {operatingRegions} -> operatingRegions) (\s@CreateIpam' {} a -> s {operatingRegions = a} :: CreateIpam) Prelude.. Lens.mapping Lens.coerced

-- | The key\/value combination of a tag assigned to the resource. Use the
-- tag key in the filter name and the tag value as the filter value. For
-- example, to find all resources that have a tag with the key @Owner@ and
-- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
-- for the filter value.
createIpam_tagSpecifications :: Lens.Lens' CreateIpam (Prelude.Maybe [TagSpecification])
createIpam_tagSpecifications = Lens.lens (\CreateIpam' {tagSpecifications} -> tagSpecifications) (\s@CreateIpam' {} a -> s {tagSpecifications = a} :: CreateIpam) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateIpam where
  type AWSResponse CreateIpam = CreateIpamResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateIpamResponse'
            Prelude.<$> (x Data..@? "ipam")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateIpam where
  hashWithSalt _salt CreateIpam' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` operatingRegions
      `Prelude.hashWithSalt` tagSpecifications

instance Prelude.NFData CreateIpam where
  rnf CreateIpam' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf operatingRegions
      `Prelude.seq` Prelude.rnf tagSpecifications

instance Data.ToHeaders CreateIpam where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateIpam where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateIpam where
  toQuery CreateIpam' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateIpam" :: Prelude.ByteString),
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

-- | /See:/ 'newCreateIpamResponse' smart constructor.
data CreateIpamResponse = CreateIpamResponse'
  { -- | Information about the IPAM created.
    ipam :: Prelude.Maybe Ipam,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIpamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipam', 'createIpamResponse_ipam' - Information about the IPAM created.
--
-- 'httpStatus', 'createIpamResponse_httpStatus' - The response's http status code.
newCreateIpamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateIpamResponse
newCreateIpamResponse pHttpStatus_ =
  CreateIpamResponse'
    { ipam = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the IPAM created.
createIpamResponse_ipam :: Lens.Lens' CreateIpamResponse (Prelude.Maybe Ipam)
createIpamResponse_ipam = Lens.lens (\CreateIpamResponse' {ipam} -> ipam) (\s@CreateIpamResponse' {} a -> s {ipam = a} :: CreateIpamResponse)

-- | The response's http status code.
createIpamResponse_httpStatus :: Lens.Lens' CreateIpamResponse Prelude.Int
createIpamResponse_httpStatus = Lens.lens (\CreateIpamResponse' {httpStatus} -> httpStatus) (\s@CreateIpamResponse' {} a -> s {httpStatus = a} :: CreateIpamResponse)

instance Prelude.NFData CreateIpamResponse where
  rnf CreateIpamResponse' {..} =
    Prelude.rnf ipam
      `Prelude.seq` Prelude.rnf httpStatus
