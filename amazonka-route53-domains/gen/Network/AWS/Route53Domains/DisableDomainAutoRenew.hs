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
-- Module      : Network.AWS.Route53Domains.DisableDomainAutoRenew
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation disables automatic renewal of domain registration for the
-- specified domain.
module Network.AWS.Route53Domains.DisableDomainAutoRenew
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | /See:/ 'newDisableDomainAutoRenew' smart constructor.
data DisableDomainAutoRenew = DisableDomainAutoRenew'
  { -- | The name of the domain that you want to disable automatic renewal for.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DisableDomainAutoRenew
newDisableDomainAutoRenew pDomainName_ =
  DisableDomainAutoRenew' {domainName = pDomainName_}

-- | The name of the domain that you want to disable automatic renewal for.
disableDomainAutoRenew_domainName :: Lens.Lens' DisableDomainAutoRenew Core.Text
disableDomainAutoRenew_domainName = Lens.lens (\DisableDomainAutoRenew' {domainName} -> domainName) (\s@DisableDomainAutoRenew' {} a -> s {domainName = a} :: DisableDomainAutoRenew)

instance Core.AWSRequest DisableDomainAutoRenew where
  type
    AWSResponse DisableDomainAutoRenew =
      DisableDomainAutoRenewResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableDomainAutoRenewResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisableDomainAutoRenew

instance Core.NFData DisableDomainAutoRenew

instance Core.ToHeaders DisableDomainAutoRenew where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Domains_v20140515.DisableDomainAutoRenew" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisableDomainAutoRenew where
  toJSON DisableDomainAutoRenew' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("DomainName" Core..= domainName)]
      )

instance Core.ToPath DisableDomainAutoRenew where
  toPath = Core.const "/"

instance Core.ToQuery DisableDomainAutoRenew where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisableDomainAutoRenewResponse' smart constructor.
data DisableDomainAutoRenewResponse = DisableDomainAutoRenewResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DisableDomainAutoRenewResponse
newDisableDomainAutoRenewResponse pHttpStatus_ =
  DisableDomainAutoRenewResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disableDomainAutoRenewResponse_httpStatus :: Lens.Lens' DisableDomainAutoRenewResponse Core.Int
disableDomainAutoRenewResponse_httpStatus = Lens.lens (\DisableDomainAutoRenewResponse' {httpStatus} -> httpStatus) (\s@DisableDomainAutoRenewResponse' {} a -> s {httpStatus = a} :: DisableDomainAutoRenewResponse)

instance Core.NFData DisableDomainAutoRenewResponse
