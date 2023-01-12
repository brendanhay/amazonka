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
-- Module      : Amazonka.Route53Domains.DisableDomainAutoRenew
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation disables automatic renewal of domain registration for the
-- specified domain.
module Amazonka.Route53Domains.DisableDomainAutoRenew
  ( -- * Creating a Request
    DisableDomainAutoRenew (..),
    newDisableDomainAutoRenew,

    -- * Request Lenses
    disableDomainAutoRenew_domainName,

    -- * Destructuring the Response
    DisableDomainAutoRenewResponse (..),
    newDisableDomainAutoRenewResponse,

    -- * Response Lenses
    disableDomainAutoRenewResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | /See:/ 'newDisableDomainAutoRenew' smart constructor.
data DisableDomainAutoRenew = DisableDomainAutoRenew'
  { -- | The name of the domain that you want to disable automatic renewal for.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableDomainAutoRenew' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'disableDomainAutoRenew_domainName' - The name of the domain that you want to disable automatic renewal for.
newDisableDomainAutoRenew ::
  -- | 'domainName'
  Prelude.Text ->
  DisableDomainAutoRenew
newDisableDomainAutoRenew pDomainName_ =
  DisableDomainAutoRenew' {domainName = pDomainName_}

-- | The name of the domain that you want to disable automatic renewal for.
disableDomainAutoRenew_domainName :: Lens.Lens' DisableDomainAutoRenew Prelude.Text
disableDomainAutoRenew_domainName = Lens.lens (\DisableDomainAutoRenew' {domainName} -> domainName) (\s@DisableDomainAutoRenew' {} a -> s {domainName = a} :: DisableDomainAutoRenew)

instance Core.AWSRequest DisableDomainAutoRenew where
  type
    AWSResponse DisableDomainAutoRenew =
      DisableDomainAutoRenewResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableDomainAutoRenewResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableDomainAutoRenew where
  hashWithSalt _salt DisableDomainAutoRenew' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData DisableDomainAutoRenew where
  rnf DisableDomainAutoRenew' {..} =
    Prelude.rnf domainName

instance Data.ToHeaders DisableDomainAutoRenew where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.DisableDomainAutoRenew" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisableDomainAutoRenew where
  toJSON DisableDomainAutoRenew' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainName" Data..= domainName)]
      )

instance Data.ToPath DisableDomainAutoRenew where
  toPath = Prelude.const "/"

instance Data.ToQuery DisableDomainAutoRenew where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableDomainAutoRenewResponse' smart constructor.
data DisableDomainAutoRenewResponse = DisableDomainAutoRenewResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableDomainAutoRenewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disableDomainAutoRenewResponse_httpStatus' - The response's http status code.
newDisableDomainAutoRenewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableDomainAutoRenewResponse
newDisableDomainAutoRenewResponse pHttpStatus_ =
  DisableDomainAutoRenewResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disableDomainAutoRenewResponse_httpStatus :: Lens.Lens' DisableDomainAutoRenewResponse Prelude.Int
disableDomainAutoRenewResponse_httpStatus = Lens.lens (\DisableDomainAutoRenewResponse' {httpStatus} -> httpStatus) (\s@DisableDomainAutoRenewResponse' {} a -> s {httpStatus = a} :: DisableDomainAutoRenewResponse)

instance
  Prelude.NFData
    DisableDomainAutoRenewResponse
  where
  rnf DisableDomainAutoRenewResponse' {..} =
    Prelude.rnf httpStatus
