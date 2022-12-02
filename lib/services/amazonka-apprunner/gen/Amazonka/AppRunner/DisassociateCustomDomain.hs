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
-- Module      : Amazonka.AppRunner.DisassociateCustomDomain
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociate a custom domain name from an App Runner service.
--
-- Certificates tracking domain validity are associated with a custom
-- domain and are stored in
-- <https://docs.aws.amazon.com/acm/latest/userguide AWS Certificate Manager (ACM)>.
-- These certificates aren\'t deleted as part of this action. App Runner
-- delays certificate deletion for 30 days after a domain is disassociated
-- from your service.
module Amazonka.AppRunner.DisassociateCustomDomain
  ( -- * Creating a Request
    DisassociateCustomDomain (..),
    newDisassociateCustomDomain,

    -- * Request Lenses
    disassociateCustomDomain_serviceArn,
    disassociateCustomDomain_domainName,

    -- * Destructuring the Response
    DisassociateCustomDomainResponse (..),
    newDisassociateCustomDomainResponse,

    -- * Response Lenses
    disassociateCustomDomainResponse_httpStatus,
    disassociateCustomDomainResponse_dNSTarget,
    disassociateCustomDomainResponse_serviceArn,
    disassociateCustomDomainResponse_customDomain,
    disassociateCustomDomainResponse_vpcDNSTargets,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateCustomDomain' smart constructor.
data DisassociateCustomDomain = DisassociateCustomDomain'
  { -- | The Amazon Resource Name (ARN) of the App Runner service that you want
    -- to disassociate a custom domain name from.
    serviceArn :: Prelude.Text,
    -- | The domain name that you want to disassociate from the App Runner
    -- service.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateCustomDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceArn', 'disassociateCustomDomain_serviceArn' - The Amazon Resource Name (ARN) of the App Runner service that you want
-- to disassociate a custom domain name from.
--
-- 'domainName', 'disassociateCustomDomain_domainName' - The domain name that you want to disassociate from the App Runner
-- service.
newDisassociateCustomDomain ::
  -- | 'serviceArn'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  DisassociateCustomDomain
newDisassociateCustomDomain pServiceArn_ pDomainName_ =
  DisassociateCustomDomain'
    { serviceArn =
        pServiceArn_,
      domainName = pDomainName_
    }

-- | The Amazon Resource Name (ARN) of the App Runner service that you want
-- to disassociate a custom domain name from.
disassociateCustomDomain_serviceArn :: Lens.Lens' DisassociateCustomDomain Prelude.Text
disassociateCustomDomain_serviceArn = Lens.lens (\DisassociateCustomDomain' {serviceArn} -> serviceArn) (\s@DisassociateCustomDomain' {} a -> s {serviceArn = a} :: DisassociateCustomDomain)

-- | The domain name that you want to disassociate from the App Runner
-- service.
disassociateCustomDomain_domainName :: Lens.Lens' DisassociateCustomDomain Prelude.Text
disassociateCustomDomain_domainName = Lens.lens (\DisassociateCustomDomain' {domainName} -> domainName) (\s@DisassociateCustomDomain' {} a -> s {domainName = a} :: DisassociateCustomDomain)

instance Core.AWSRequest DisassociateCustomDomain where
  type
    AWSResponse DisassociateCustomDomain =
      DisassociateCustomDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateCustomDomainResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "DNSTarget")
            Prelude.<*> (x Data..:> "ServiceArn")
            Prelude.<*> (x Data..:> "CustomDomain")
            Prelude.<*> (x Data..?> "VpcDNSTargets" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable DisassociateCustomDomain where
  hashWithSalt _salt DisassociateCustomDomain' {..} =
    _salt `Prelude.hashWithSalt` serviceArn
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DisassociateCustomDomain where
  rnf DisassociateCustomDomain' {..} =
    Prelude.rnf serviceArn
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders DisassociateCustomDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AppRunner.DisassociateCustomDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateCustomDomain where
  toJSON DisassociateCustomDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ServiceArn" Data..= serviceArn),
            Prelude.Just ("DomainName" Data..= domainName)
          ]
      )

instance Data.ToPath DisassociateCustomDomain where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateCustomDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateCustomDomainResponse' smart constructor.
data DisassociateCustomDomainResponse = DisassociateCustomDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The App Runner subdomain of the App Runner service. The disassociated
    -- custom domain name was mapped to this target name.
    dNSTarget :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the App Runner service that a custom
    -- domain name is disassociated from.
    serviceArn :: Prelude.Text,
    -- | A description of the domain name that\'s being disassociated.
    customDomain :: CustomDomain,
    -- | DNS Target records for the custom domains of this Amazon VPC.
    vpcDNSTargets :: [VpcDNSTarget]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateCustomDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateCustomDomainResponse_httpStatus' - The response's http status code.
--
-- 'dNSTarget', 'disassociateCustomDomainResponse_dNSTarget' - The App Runner subdomain of the App Runner service. The disassociated
-- custom domain name was mapped to this target name.
--
-- 'serviceArn', 'disassociateCustomDomainResponse_serviceArn' - The Amazon Resource Name (ARN) of the App Runner service that a custom
-- domain name is disassociated from.
--
-- 'customDomain', 'disassociateCustomDomainResponse_customDomain' - A description of the domain name that\'s being disassociated.
--
-- 'vpcDNSTargets', 'disassociateCustomDomainResponse_vpcDNSTargets' - DNS Target records for the custom domains of this Amazon VPC.
newDisassociateCustomDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'dNSTarget'
  Prelude.Text ->
  -- | 'serviceArn'
  Prelude.Text ->
  -- | 'customDomain'
  CustomDomain ->
  DisassociateCustomDomainResponse
newDisassociateCustomDomainResponse
  pHttpStatus_
  pDNSTarget_
  pServiceArn_
  pCustomDomain_ =
    DisassociateCustomDomainResponse'
      { httpStatus =
          pHttpStatus_,
        dNSTarget = pDNSTarget_,
        serviceArn = pServiceArn_,
        customDomain = pCustomDomain_,
        vpcDNSTargets = Prelude.mempty
      }

-- | The response's http status code.
disassociateCustomDomainResponse_httpStatus :: Lens.Lens' DisassociateCustomDomainResponse Prelude.Int
disassociateCustomDomainResponse_httpStatus = Lens.lens (\DisassociateCustomDomainResponse' {httpStatus} -> httpStatus) (\s@DisassociateCustomDomainResponse' {} a -> s {httpStatus = a} :: DisassociateCustomDomainResponse)

-- | The App Runner subdomain of the App Runner service. The disassociated
-- custom domain name was mapped to this target name.
disassociateCustomDomainResponse_dNSTarget :: Lens.Lens' DisassociateCustomDomainResponse Prelude.Text
disassociateCustomDomainResponse_dNSTarget = Lens.lens (\DisassociateCustomDomainResponse' {dNSTarget} -> dNSTarget) (\s@DisassociateCustomDomainResponse' {} a -> s {dNSTarget = a} :: DisassociateCustomDomainResponse)

-- | The Amazon Resource Name (ARN) of the App Runner service that a custom
-- domain name is disassociated from.
disassociateCustomDomainResponse_serviceArn :: Lens.Lens' DisassociateCustomDomainResponse Prelude.Text
disassociateCustomDomainResponse_serviceArn = Lens.lens (\DisassociateCustomDomainResponse' {serviceArn} -> serviceArn) (\s@DisassociateCustomDomainResponse' {} a -> s {serviceArn = a} :: DisassociateCustomDomainResponse)

-- | A description of the domain name that\'s being disassociated.
disassociateCustomDomainResponse_customDomain :: Lens.Lens' DisassociateCustomDomainResponse CustomDomain
disassociateCustomDomainResponse_customDomain = Lens.lens (\DisassociateCustomDomainResponse' {customDomain} -> customDomain) (\s@DisassociateCustomDomainResponse' {} a -> s {customDomain = a} :: DisassociateCustomDomainResponse)

-- | DNS Target records for the custom domains of this Amazon VPC.
disassociateCustomDomainResponse_vpcDNSTargets :: Lens.Lens' DisassociateCustomDomainResponse [VpcDNSTarget]
disassociateCustomDomainResponse_vpcDNSTargets = Lens.lens (\DisassociateCustomDomainResponse' {vpcDNSTargets} -> vpcDNSTargets) (\s@DisassociateCustomDomainResponse' {} a -> s {vpcDNSTargets = a} :: DisassociateCustomDomainResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DisassociateCustomDomainResponse
  where
  rnf DisassociateCustomDomainResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf dNSTarget
      `Prelude.seq` Prelude.rnf serviceArn
      `Prelude.seq` Prelude.rnf customDomain
      `Prelude.seq` Prelude.rnf vpcDNSTargets
