{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | /See:/ 'newDisableDomainAutoRenew' smart constructor.
data DisableDomainAutoRenew = DisableDomainAutoRenew'
  { -- | The name of the domain that you want to disable automatic renewal for.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DisableDomainAutoRenew where
  type
    Rs DisableDomainAutoRenew =
      DisableDomainAutoRenewResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableDomainAutoRenewResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableDomainAutoRenew

instance Prelude.NFData DisableDomainAutoRenew

instance Prelude.ToHeaders DisableDomainAutoRenew where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Route53Domains_v20140515.DisableDomainAutoRenew" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisableDomainAutoRenew where
  toJSON DisableDomainAutoRenew' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainName" Prelude..= domainName)]
      )

instance Prelude.ToPath DisableDomainAutoRenew where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisableDomainAutoRenew where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableDomainAutoRenewResponse' smart constructor.
data DisableDomainAutoRenewResponse = DisableDomainAutoRenewResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
