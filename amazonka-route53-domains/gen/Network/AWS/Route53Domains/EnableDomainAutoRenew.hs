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
-- Module      : Network.AWS.Route53Domains.EnableDomainAutoRenew
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation configures Amazon Route 53 to automatically renew the
-- specified domain before the domain registration expires. The cost of
-- renewing your domain registration is billed to your AWS account.
--
-- The period during which you can renew a domain name varies by TLD. For a
-- list of TLDs and their renewal policies, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains That You Can Register with Amazon Route 53>
-- in the /Amazon Route 53 Developer Guide/. Route 53 requires that you
-- renew before the end of the renewal period so we can complete processing
-- before the deadline.
module Network.AWS.Route53Domains.EnableDomainAutoRenew
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | /See:/ 'newEnableDomainAutoRenew' smart constructor.
data EnableDomainAutoRenew = EnableDomainAutoRenew'
  { -- | The name of the domain that you want to enable automatic renewal for.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest EnableDomainAutoRenew where
  type
    Rs EnableDomainAutoRenew =
      EnableDomainAutoRenewResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableDomainAutoRenewResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableDomainAutoRenew

instance Prelude.NFData EnableDomainAutoRenew

instance Prelude.ToHeaders EnableDomainAutoRenew where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Route53Domains_v20140515.EnableDomainAutoRenew" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON EnableDomainAutoRenew where
  toJSON EnableDomainAutoRenew' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainName" Prelude..= domainName)]
      )

instance Prelude.ToPath EnableDomainAutoRenew where
  toPath = Prelude.const "/"

instance Prelude.ToQuery EnableDomainAutoRenew where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableDomainAutoRenewResponse' smart constructor.
data EnableDomainAutoRenewResponse = EnableDomainAutoRenewResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData EnableDomainAutoRenewResponse
