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
-- Module      : Network.AWS.EC2.ProvisionByoipCidr
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions an IPv4 or IPv6 address range for use with your AWS resources
-- through bring your own IP addresses (BYOIP) and creates a corresponding
-- address pool. After the address range is provisioned, it is ready to be
-- advertised using AdvertiseByoipCidr.
--
-- AWS verifies that you own the address range and are authorized to
-- advertise it. You must ensure that the address range is registered to
-- you and that you created an RPKI ROA to authorize Amazon ASNs 16509 and
-- 14618 to advertise the address range. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-byoip.html Bring Your Own IP Addresses (BYOIP)>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Provisioning an address range is an asynchronous operation, so the call
-- returns immediately, but the address range is not ready to use until its
-- status changes from @pending-provision@ to @provisioned@. To monitor the
-- status of an address range, use DescribeByoipCidrs. To allocate an
-- Elastic IP address from your IPv4 address pool, use AllocateAddress with
-- either the specific address from the address pool or the ID of the
-- address pool.
module Network.AWS.EC2.ProvisionByoipCidr
  ( -- * Creating a Request
    ProvisionByoipCidr (..),
    newProvisionByoipCidr,

    -- * Request Lenses
    provisionByoipCidr_dryRun,
    provisionByoipCidr_cidrAuthorizationContext,
    provisionByoipCidr_publiclyAdvertisable,
    provisionByoipCidr_description,
    provisionByoipCidr_poolTagSpecifications,
    provisionByoipCidr_cidr,

    -- * Destructuring the Response
    ProvisionByoipCidrResponse (..),
    newProvisionByoipCidrResponse,

    -- * Response Lenses
    provisionByoipCidrResponse_byoipCidr,
    provisionByoipCidrResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newProvisionByoipCidr' smart constructor.
data ProvisionByoipCidr = ProvisionByoipCidr'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | A signed document that proves that you are authorized to bring the
    -- specified IP address range to Amazon using BYOIP.
    cidrAuthorizationContext :: Prelude.Maybe CidrAuthorizationContext,
    -- | (IPv6 only) Indicate whether the address range will be publicly
    -- advertised to the internet.
    --
    -- Default: true
    publiclyAdvertisable :: Prelude.Maybe Prelude.Bool,
    -- | A description for the address range and the address pool.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags to apply to the address pool.
    poolTagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The public IPv4 or IPv6 address range, in CIDR notation. The most
    -- specific IPv4 prefix that you can specify is \/24. The most specific
    -- IPv6 prefix you can specify is \/56. The address range cannot overlap
    -- with another address range that you\'ve brought to this or another
    -- Region.
    cidr :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionByoipCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'provisionByoipCidr_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'cidrAuthorizationContext', 'provisionByoipCidr_cidrAuthorizationContext' - A signed document that proves that you are authorized to bring the
-- specified IP address range to Amazon using BYOIP.
--
-- 'publiclyAdvertisable', 'provisionByoipCidr_publiclyAdvertisable' - (IPv6 only) Indicate whether the address range will be publicly
-- advertised to the internet.
--
-- Default: true
--
-- 'description', 'provisionByoipCidr_description' - A description for the address range and the address pool.
--
-- 'poolTagSpecifications', 'provisionByoipCidr_poolTagSpecifications' - The tags to apply to the address pool.
--
-- 'cidr', 'provisionByoipCidr_cidr' - The public IPv4 or IPv6 address range, in CIDR notation. The most
-- specific IPv4 prefix that you can specify is \/24. The most specific
-- IPv6 prefix you can specify is \/56. The address range cannot overlap
-- with another address range that you\'ve brought to this or another
-- Region.
newProvisionByoipCidr ::
  -- | 'cidr'
  Prelude.Text ->
  ProvisionByoipCidr
newProvisionByoipCidr pCidr_ =
  ProvisionByoipCidr'
    { dryRun = Prelude.Nothing,
      cidrAuthorizationContext = Prelude.Nothing,
      publiclyAdvertisable = Prelude.Nothing,
      description = Prelude.Nothing,
      poolTagSpecifications = Prelude.Nothing,
      cidr = pCidr_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
provisionByoipCidr_dryRun :: Lens.Lens' ProvisionByoipCidr (Prelude.Maybe Prelude.Bool)
provisionByoipCidr_dryRun = Lens.lens (\ProvisionByoipCidr' {dryRun} -> dryRun) (\s@ProvisionByoipCidr' {} a -> s {dryRun = a} :: ProvisionByoipCidr)

-- | A signed document that proves that you are authorized to bring the
-- specified IP address range to Amazon using BYOIP.
provisionByoipCidr_cidrAuthorizationContext :: Lens.Lens' ProvisionByoipCidr (Prelude.Maybe CidrAuthorizationContext)
provisionByoipCidr_cidrAuthorizationContext = Lens.lens (\ProvisionByoipCidr' {cidrAuthorizationContext} -> cidrAuthorizationContext) (\s@ProvisionByoipCidr' {} a -> s {cidrAuthorizationContext = a} :: ProvisionByoipCidr)

-- | (IPv6 only) Indicate whether the address range will be publicly
-- advertised to the internet.
--
-- Default: true
provisionByoipCidr_publiclyAdvertisable :: Lens.Lens' ProvisionByoipCidr (Prelude.Maybe Prelude.Bool)
provisionByoipCidr_publiclyAdvertisable = Lens.lens (\ProvisionByoipCidr' {publiclyAdvertisable} -> publiclyAdvertisable) (\s@ProvisionByoipCidr' {} a -> s {publiclyAdvertisable = a} :: ProvisionByoipCidr)

-- | A description for the address range and the address pool.
provisionByoipCidr_description :: Lens.Lens' ProvisionByoipCidr (Prelude.Maybe Prelude.Text)
provisionByoipCidr_description = Lens.lens (\ProvisionByoipCidr' {description} -> description) (\s@ProvisionByoipCidr' {} a -> s {description = a} :: ProvisionByoipCidr)

-- | The tags to apply to the address pool.
provisionByoipCidr_poolTagSpecifications :: Lens.Lens' ProvisionByoipCidr (Prelude.Maybe [TagSpecification])
provisionByoipCidr_poolTagSpecifications = Lens.lens (\ProvisionByoipCidr' {poolTagSpecifications} -> poolTagSpecifications) (\s@ProvisionByoipCidr' {} a -> s {poolTagSpecifications = a} :: ProvisionByoipCidr) Prelude.. Lens.mapping Lens._Coerce

-- | The public IPv4 or IPv6 address range, in CIDR notation. The most
-- specific IPv4 prefix that you can specify is \/24. The most specific
-- IPv6 prefix you can specify is \/56. The address range cannot overlap
-- with another address range that you\'ve brought to this or another
-- Region.
provisionByoipCidr_cidr :: Lens.Lens' ProvisionByoipCidr Prelude.Text
provisionByoipCidr_cidr = Lens.lens (\ProvisionByoipCidr' {cidr} -> cidr) (\s@ProvisionByoipCidr' {} a -> s {cidr = a} :: ProvisionByoipCidr)

instance Core.AWSRequest ProvisionByoipCidr where
  type
    AWSResponse ProvisionByoipCidr =
      ProvisionByoipCidrResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ProvisionByoipCidrResponse'
            Prelude.<$> (x Core..@? "byoipCidr")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ProvisionByoipCidr

instance Prelude.NFData ProvisionByoipCidr

instance Core.ToHeaders ProvisionByoipCidr where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ProvisionByoipCidr where
  toPath = Prelude.const "/"

instance Core.ToQuery ProvisionByoipCidr where
  toQuery ProvisionByoipCidr' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ProvisionByoipCidr" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "CidrAuthorizationContext"
          Core.=: cidrAuthorizationContext,
        "PubliclyAdvertisable" Core.=: publiclyAdvertisable,
        "Description" Core.=: description,
        Core.toQuery
          ( Core.toQueryList "PoolTagSpecification"
              Prelude.<$> poolTagSpecifications
          ),
        "Cidr" Core.=: cidr
      ]

-- | /See:/ 'newProvisionByoipCidrResponse' smart constructor.
data ProvisionByoipCidrResponse = ProvisionByoipCidrResponse'
  { -- | Information about the address range.
    byoipCidr :: Prelude.Maybe ByoipCidr,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionByoipCidrResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'byoipCidr', 'provisionByoipCidrResponse_byoipCidr' - Information about the address range.
--
-- 'httpStatus', 'provisionByoipCidrResponse_httpStatus' - The response's http status code.
newProvisionByoipCidrResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ProvisionByoipCidrResponse
newProvisionByoipCidrResponse pHttpStatus_ =
  ProvisionByoipCidrResponse'
    { byoipCidr =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the address range.
provisionByoipCidrResponse_byoipCidr :: Lens.Lens' ProvisionByoipCidrResponse (Prelude.Maybe ByoipCidr)
provisionByoipCidrResponse_byoipCidr = Lens.lens (\ProvisionByoipCidrResponse' {byoipCidr} -> byoipCidr) (\s@ProvisionByoipCidrResponse' {} a -> s {byoipCidr = a} :: ProvisionByoipCidrResponse)

-- | The response's http status code.
provisionByoipCidrResponse_httpStatus :: Lens.Lens' ProvisionByoipCidrResponse Prelude.Int
provisionByoipCidrResponse_httpStatus = Lens.lens (\ProvisionByoipCidrResponse' {httpStatus} -> httpStatus) (\s@ProvisionByoipCidrResponse' {} a -> s {httpStatus = a} :: ProvisionByoipCidrResponse)

instance Prelude.NFData ProvisionByoipCidrResponse
