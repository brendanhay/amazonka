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
-- Module      : Amazonka.WorkLink.DescribeDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the domain.
module Amazonka.WorkLink.DescribeDomain
  ( -- * Creating a Request
    DescribeDomain (..),
    newDescribeDomain,

    -- * Request Lenses
    describeDomain_fleetArn,
    describeDomain_domainName,

    -- * Destructuring the Response
    DescribeDomainResponse (..),
    newDescribeDomainResponse,

    -- * Response Lenses
    describeDomainResponse_domainStatus,
    describeDomainResponse_acmCertificateArn,
    describeDomainResponse_createdTime,
    describeDomainResponse_domainName,
    describeDomainResponse_displayName,
    describeDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newDescribeDomain' smart constructor.
data DescribeDomain = DescribeDomain'
  { -- | The ARN of the fleet.
    fleetArn :: Prelude.Text,
    -- | The name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'describeDomain_fleetArn' - The ARN of the fleet.
--
-- 'domainName', 'describeDomain_domainName' - The name of the domain.
newDescribeDomain ::
  -- | 'fleetArn'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  DescribeDomain
newDescribeDomain pFleetArn_ pDomainName_ =
  DescribeDomain'
    { fleetArn = pFleetArn_,
      domainName = pDomainName_
    }

-- | The ARN of the fleet.
describeDomain_fleetArn :: Lens.Lens' DescribeDomain Prelude.Text
describeDomain_fleetArn = Lens.lens (\DescribeDomain' {fleetArn} -> fleetArn) (\s@DescribeDomain' {} a -> s {fleetArn = a} :: DescribeDomain)

-- | The name of the domain.
describeDomain_domainName :: Lens.Lens' DescribeDomain Prelude.Text
describeDomain_domainName = Lens.lens (\DescribeDomain' {domainName} -> domainName) (\s@DescribeDomain' {} a -> s {domainName = a} :: DescribeDomain)

instance Core.AWSRequest DescribeDomain where
  type
    AWSResponse DescribeDomain =
      DescribeDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDomainResponse'
            Prelude.<$> (x Core..?> "DomainStatus")
            Prelude.<*> (x Core..?> "AcmCertificateArn")
            Prelude.<*> (x Core..?> "CreatedTime")
            Prelude.<*> (x Core..?> "DomainName")
            Prelude.<*> (x Core..?> "DisplayName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDomain where
  hashWithSalt salt' DescribeDomain' {..} =
    salt' `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` fleetArn

instance Prelude.NFData DescribeDomain where
  rnf DescribeDomain' {..} =
    Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf domainName

instance Core.ToHeaders DescribeDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeDomain where
  toJSON DescribeDomain' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetArn" Core..= fleetArn),
            Prelude.Just ("DomainName" Core..= domainName)
          ]
      )

instance Core.ToPath DescribeDomain where
  toPath = Prelude.const "/describeDomain"

instance Core.ToQuery DescribeDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDomainResponse' smart constructor.
data DescribeDomainResponse = DescribeDomainResponse'
  { -- | The current state for the domain.
    domainStatus :: Prelude.Maybe DomainStatus,
    -- | The ARN of an issued ACM certificate that is valid for the domain being
    -- associated.
    acmCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The time that the domain was added.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the domain.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainStatus', 'describeDomainResponse_domainStatus' - The current state for the domain.
--
-- 'acmCertificateArn', 'describeDomainResponse_acmCertificateArn' - The ARN of an issued ACM certificate that is valid for the domain being
-- associated.
--
-- 'createdTime', 'describeDomainResponse_createdTime' - The time that the domain was added.
--
-- 'domainName', 'describeDomainResponse_domainName' - The name of the domain.
--
-- 'displayName', 'describeDomainResponse_displayName' - The name to display.
--
-- 'httpStatus', 'describeDomainResponse_httpStatus' - The response's http status code.
newDescribeDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDomainResponse
newDescribeDomainResponse pHttpStatus_ =
  DescribeDomainResponse'
    { domainStatus =
        Prelude.Nothing,
      acmCertificateArn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      domainName = Prelude.Nothing,
      displayName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current state for the domain.
describeDomainResponse_domainStatus :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe DomainStatus)
describeDomainResponse_domainStatus = Lens.lens (\DescribeDomainResponse' {domainStatus} -> domainStatus) (\s@DescribeDomainResponse' {} a -> s {domainStatus = a} :: DescribeDomainResponse)

-- | The ARN of an issued ACM certificate that is valid for the domain being
-- associated.
describeDomainResponse_acmCertificateArn :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe Prelude.Text)
describeDomainResponse_acmCertificateArn = Lens.lens (\DescribeDomainResponse' {acmCertificateArn} -> acmCertificateArn) (\s@DescribeDomainResponse' {} a -> s {acmCertificateArn = a} :: DescribeDomainResponse)

-- | The time that the domain was added.
describeDomainResponse_createdTime :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe Prelude.UTCTime)
describeDomainResponse_createdTime = Lens.lens (\DescribeDomainResponse' {createdTime} -> createdTime) (\s@DescribeDomainResponse' {} a -> s {createdTime = a} :: DescribeDomainResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the domain.
describeDomainResponse_domainName :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe Prelude.Text)
describeDomainResponse_domainName = Lens.lens (\DescribeDomainResponse' {domainName} -> domainName) (\s@DescribeDomainResponse' {} a -> s {domainName = a} :: DescribeDomainResponse)

-- | The name to display.
describeDomainResponse_displayName :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe Prelude.Text)
describeDomainResponse_displayName = Lens.lens (\DescribeDomainResponse' {displayName} -> displayName) (\s@DescribeDomainResponse' {} a -> s {displayName = a} :: DescribeDomainResponse)

-- | The response's http status code.
describeDomainResponse_httpStatus :: Lens.Lens' DescribeDomainResponse Prelude.Int
describeDomainResponse_httpStatus = Lens.lens (\DescribeDomainResponse' {httpStatus} -> httpStatus) (\s@DescribeDomainResponse' {} a -> s {httpStatus = a} :: DescribeDomainResponse)

instance Prelude.NFData DescribeDomainResponse where
  rnf DescribeDomainResponse' {..} =
    Prelude.rnf domainStatus
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf acmCertificateArn
