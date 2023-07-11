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
-- Module      : Amazonka.Route53Domains.EnableDomainAutoRenew
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation configures Amazon Route 53 to automatically renew the
-- specified domain before the domain registration expires. The cost of
-- renewing your domain registration is billed to your Amazon Web Services
-- account.
--
-- The period during which you can renew a domain name varies by TLD. For a
-- list of TLDs and their renewal policies, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains That You Can Register with Amazon Route 53>
-- in the /Amazon Route 53 Developer Guide/. Route 53 requires that you
-- renew before the end of the renewal period so we can complete processing
-- before the deadline.
module Amazonka.Route53Domains.EnableDomainAutoRenew
  ( -- * Creating a Request
    EnableDomainAutoRenew (..),
    newEnableDomainAutoRenew,

    -- * Request Lenses
    enableDomainAutoRenew_domainName,

    -- * Destructuring the Response
    EnableDomainAutoRenewResponse (..),
    newEnableDomainAutoRenewResponse,

    -- * Response Lenses
    enableDomainAutoRenewResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | /See:/ 'newEnableDomainAutoRenew' smart constructor.
data EnableDomainAutoRenew = EnableDomainAutoRenew'
  { -- | The name of the domain that you want to enable automatic renewal for.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableDomainAutoRenew' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'enableDomainAutoRenew_domainName' - The name of the domain that you want to enable automatic renewal for.
newEnableDomainAutoRenew ::
  -- | 'domainName'
  Prelude.Text ->
  EnableDomainAutoRenew
newEnableDomainAutoRenew pDomainName_ =
  EnableDomainAutoRenew' {domainName = pDomainName_}

-- | The name of the domain that you want to enable automatic renewal for.
enableDomainAutoRenew_domainName :: Lens.Lens' EnableDomainAutoRenew Prelude.Text
enableDomainAutoRenew_domainName = Lens.lens (\EnableDomainAutoRenew' {domainName} -> domainName) (\s@EnableDomainAutoRenew' {} a -> s {domainName = a} :: EnableDomainAutoRenew)

instance Core.AWSRequest EnableDomainAutoRenew where
  type
    AWSResponse EnableDomainAutoRenew =
      EnableDomainAutoRenewResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableDomainAutoRenewResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableDomainAutoRenew where
  hashWithSalt _salt EnableDomainAutoRenew' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData EnableDomainAutoRenew where
  rnf EnableDomainAutoRenew' {..} =
    Prelude.rnf domainName

instance Data.ToHeaders EnableDomainAutoRenew where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.EnableDomainAutoRenew" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EnableDomainAutoRenew where
  toJSON EnableDomainAutoRenew' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainName" Data..= domainName)]
      )

instance Data.ToPath EnableDomainAutoRenew where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableDomainAutoRenew where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableDomainAutoRenewResponse' smart constructor.
data EnableDomainAutoRenewResponse = EnableDomainAutoRenewResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableDomainAutoRenewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableDomainAutoRenewResponse_httpStatus' - The response's http status code.
newEnableDomainAutoRenewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableDomainAutoRenewResponse
newEnableDomainAutoRenewResponse pHttpStatus_ =
  EnableDomainAutoRenewResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
enableDomainAutoRenewResponse_httpStatus :: Lens.Lens' EnableDomainAutoRenewResponse Prelude.Int
enableDomainAutoRenewResponse_httpStatus = Lens.lens (\EnableDomainAutoRenewResponse' {httpStatus} -> httpStatus) (\s@EnableDomainAutoRenewResponse' {} a -> s {httpStatus = a} :: EnableDomainAutoRenewResponse)

instance Prelude.NFData EnableDomainAutoRenewResponse where
  rnf EnableDomainAutoRenewResponse' {..} =
    Prelude.rnf httpStatus
