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
-- Module      : Amazonka.EC2.CreateDhcpOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a set of DHCP options for your VPC. After creating the set, you
-- must associate it with the VPC, causing all existing and new instances
-- that you launch in the VPC to use this set of DHCP options. The
-- following are the individual DHCP options you can specify. For more
-- information about the options, see
-- <http://www.ietf.org/rfc/rfc2132.txt RFC 2132>.
--
-- -   @domain-name-servers@ - The IP addresses of up to four domain name
--     servers, or AmazonProvidedDNS. The default DHCP option set specifies
--     AmazonProvidedDNS. If specifying more than one domain name server,
--     specify the IP addresses in a single parameter, separated by commas.
--     To have your instance receive a custom DNS hostname as specified in
--     @domain-name@, you must set @domain-name-servers@ to a custom DNS
--     server.
--
-- -   @domain-name@ - If you\'re using AmazonProvidedDNS in @us-east-1@,
--     specify @ec2.internal@. If you\'re using AmazonProvidedDNS in
--     another Region, specify @region.compute.internal@ (for example,
--     @ap-northeast-1.compute.internal@). Otherwise, specify a domain name
--     (for example, @ExampleCompany.com@). This value is used to complete
--     unqualified DNS hostnames. __Important__: Some Linux operating
--     systems accept multiple domain names separated by spaces. However,
--     Windows and other Linux operating systems treat the value as a
--     single domain, which results in unexpected behavior. If your DHCP
--     options set is associated with a VPC that has instances with
--     multiple operating systems, specify only one domain name.
--
-- -   @ntp-servers@ - The IP addresses of up to four Network Time Protocol
--     (NTP) servers.
--
-- -   @netbios-name-servers@ - The IP addresses of up to four NetBIOS name
--     servers.
--
-- -   @netbios-node-type@ - The NetBIOS node type (1, 2, 4, or 8). We
--     recommend that you specify 2 (broadcast and multicast are not
--     currently supported). For more information about these node types,
--     see <http://www.ietf.org/rfc/rfc2132.txt RFC 2132>.
--
-- Your VPC automatically starts out with a set of DHCP options that
-- includes only a DNS server that we provide (AmazonProvidedDNS). If you
-- create a set of options, and if your VPC has an internet gateway, make
-- sure to set the @domain-name-servers@ option either to
-- @AmazonProvidedDNS@ or to a domain name server of your choice. For more
-- information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_DHCP_Options.html DHCP options sets>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Amazonka.EC2.CreateDhcpOptions
  ( -- * Creating a Request
    CreateDhcpOptions (..),
    newCreateDhcpOptions,

    -- * Request Lenses
    createDhcpOptions_dryRun,
    createDhcpOptions_tagSpecifications,
    createDhcpOptions_dhcpConfigurations,

    -- * Destructuring the Response
    CreateDhcpOptionsResponse (..),
    newCreateDhcpOptionsResponse,

    -- * Response Lenses
    createDhcpOptionsResponse_dhcpOptions,
    createDhcpOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDhcpOptions' smart constructor.
data CreateDhcpOptions = CreateDhcpOptions'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to assign to the DHCP option.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | A DHCP configuration option.
    dhcpConfigurations :: [NewDhcpConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDhcpOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createDhcpOptions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createDhcpOptions_tagSpecifications' - The tags to assign to the DHCP option.
--
-- 'dhcpConfigurations', 'createDhcpOptions_dhcpConfigurations' - A DHCP configuration option.
newCreateDhcpOptions ::
  CreateDhcpOptions
newCreateDhcpOptions =
  CreateDhcpOptions'
    { dryRun = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      dhcpConfigurations = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createDhcpOptions_dryRun :: Lens.Lens' CreateDhcpOptions (Prelude.Maybe Prelude.Bool)
createDhcpOptions_dryRun = Lens.lens (\CreateDhcpOptions' {dryRun} -> dryRun) (\s@CreateDhcpOptions' {} a -> s {dryRun = a} :: CreateDhcpOptions)

-- | The tags to assign to the DHCP option.
createDhcpOptions_tagSpecifications :: Lens.Lens' CreateDhcpOptions (Prelude.Maybe [TagSpecification])
createDhcpOptions_tagSpecifications = Lens.lens (\CreateDhcpOptions' {tagSpecifications} -> tagSpecifications) (\s@CreateDhcpOptions' {} a -> s {tagSpecifications = a} :: CreateDhcpOptions) Prelude.. Lens.mapping Lens.coerced

-- | A DHCP configuration option.
createDhcpOptions_dhcpConfigurations :: Lens.Lens' CreateDhcpOptions [NewDhcpConfiguration]
createDhcpOptions_dhcpConfigurations = Lens.lens (\CreateDhcpOptions' {dhcpConfigurations} -> dhcpConfigurations) (\s@CreateDhcpOptions' {} a -> s {dhcpConfigurations = a} :: CreateDhcpOptions) Prelude.. Lens.coerced

instance Core.AWSRequest CreateDhcpOptions where
  type
    AWSResponse CreateDhcpOptions =
      CreateDhcpOptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateDhcpOptionsResponse'
            Prelude.<$> (x Data..@? "dhcpOptions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDhcpOptions where
  hashWithSalt _salt CreateDhcpOptions' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` dhcpConfigurations

instance Prelude.NFData CreateDhcpOptions where
  rnf CreateDhcpOptions' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf dhcpConfigurations

instance Data.ToHeaders CreateDhcpOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateDhcpOptions where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDhcpOptions where
  toQuery CreateDhcpOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateDhcpOptions" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        Data.toQueryList
          "DhcpConfiguration"
          dhcpConfigurations
      ]

-- | /See:/ 'newCreateDhcpOptionsResponse' smart constructor.
data CreateDhcpOptionsResponse = CreateDhcpOptionsResponse'
  { -- | A set of DHCP options.
    dhcpOptions :: Prelude.Maybe DhcpOptions,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDhcpOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dhcpOptions', 'createDhcpOptionsResponse_dhcpOptions' - A set of DHCP options.
--
-- 'httpStatus', 'createDhcpOptionsResponse_httpStatus' - The response's http status code.
newCreateDhcpOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDhcpOptionsResponse
newCreateDhcpOptionsResponse pHttpStatus_ =
  CreateDhcpOptionsResponse'
    { dhcpOptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A set of DHCP options.
createDhcpOptionsResponse_dhcpOptions :: Lens.Lens' CreateDhcpOptionsResponse (Prelude.Maybe DhcpOptions)
createDhcpOptionsResponse_dhcpOptions = Lens.lens (\CreateDhcpOptionsResponse' {dhcpOptions} -> dhcpOptions) (\s@CreateDhcpOptionsResponse' {} a -> s {dhcpOptions = a} :: CreateDhcpOptionsResponse)

-- | The response's http status code.
createDhcpOptionsResponse_httpStatus :: Lens.Lens' CreateDhcpOptionsResponse Prelude.Int
createDhcpOptionsResponse_httpStatus = Lens.lens (\CreateDhcpOptionsResponse' {httpStatus} -> httpStatus) (\s@CreateDhcpOptionsResponse' {} a -> s {httpStatus = a} :: CreateDhcpOptionsResponse)

instance Prelude.NFData CreateDhcpOptionsResponse where
  rnf CreateDhcpOptionsResponse' {..} =
    Prelude.rnf dhcpOptions
      `Prelude.seq` Prelude.rnf httpStatus
