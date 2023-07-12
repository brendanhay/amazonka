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
-- Module      : Amazonka.Route53Domains.PushDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves a domain from Amazon Web Services to another registrar.
--
-- Supported actions:
--
-- -   Changes the IPS tags of a .uk domain, and pushes it to transit.
--     Transit means that the domain is ready to be transferred to another
--     registrar.
module Amazonka.Route53Domains.PushDomain
  ( -- * Creating a Request
    PushDomain (..),
    newPushDomain,

    -- * Request Lenses
    pushDomain_domainName,
    pushDomain_target,

    -- * Destructuring the Response
    PushDomainResponse (..),
    newPushDomainResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | /See:/ 'newPushDomain' smart constructor.
data PushDomain = PushDomain'
  { -- | Name of the domain.
    domainName :: Prelude.Text,
    -- | New IPS tag for the domain.
    target :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PushDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'pushDomain_domainName' - Name of the domain.
--
-- 'target', 'pushDomain_target' - New IPS tag for the domain.
newPushDomain ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'target'
  Prelude.Text ->
  PushDomain
newPushDomain pDomainName_ pTarget_ =
  PushDomain'
    { domainName = pDomainName_,
      target = pTarget_
    }

-- | Name of the domain.
pushDomain_domainName :: Lens.Lens' PushDomain Prelude.Text
pushDomain_domainName = Lens.lens (\PushDomain' {domainName} -> domainName) (\s@PushDomain' {} a -> s {domainName = a} :: PushDomain)

-- | New IPS tag for the domain.
pushDomain_target :: Lens.Lens' PushDomain Prelude.Text
pushDomain_target = Lens.lens (\PushDomain' {target} -> target) (\s@PushDomain' {} a -> s {target = a} :: PushDomain)

instance Core.AWSRequest PushDomain where
  type AWSResponse PushDomain = PushDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull PushDomainResponse'

instance Prelude.Hashable PushDomain where
  hashWithSalt _salt PushDomain' {..} =
    _salt
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` target

instance Prelude.NFData PushDomain where
  rnf PushDomain' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf target

instance Data.ToHeaders PushDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.PushDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PushDomain where
  toJSON PushDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainName" Data..= domainName),
            Prelude.Just ("Target" Data..= target)
          ]
      )

instance Data.ToPath PushDomain where
  toPath = Prelude.const "/"

instance Data.ToQuery PushDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPushDomainResponse' smart constructor.
data PushDomainResponse = PushDomainResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PushDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPushDomainResponse ::
  PushDomainResponse
newPushDomainResponse = PushDomainResponse'

instance Prelude.NFData PushDomainResponse where
  rnf _ = ()
