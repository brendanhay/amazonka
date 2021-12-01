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
-- Module      : Amazonka.WorkLink.DisassociateDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a domain from Amazon WorkLink. End users lose the ability
-- to access the domain with Amazon WorkLink.
module Amazonka.WorkLink.DisassociateDomain
  ( -- * Creating a Request
    DisassociateDomain (..),
    newDisassociateDomain,

    -- * Request Lenses
    disassociateDomain_fleetArn,
    disassociateDomain_domainName,

    -- * Destructuring the Response
    DisassociateDomainResponse (..),
    newDisassociateDomainResponse,

    -- * Response Lenses
    disassociateDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newDisassociateDomain' smart constructor.
data DisassociateDomain = DisassociateDomain'
  { -- | The ARN of the fleet.
    fleetArn :: Prelude.Text,
    -- | The name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'disassociateDomain_fleetArn' - The ARN of the fleet.
--
-- 'domainName', 'disassociateDomain_domainName' - The name of the domain.
newDisassociateDomain ::
  -- | 'fleetArn'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  DisassociateDomain
newDisassociateDomain pFleetArn_ pDomainName_ =
  DisassociateDomain'
    { fleetArn = pFleetArn_,
      domainName = pDomainName_
    }

-- | The ARN of the fleet.
disassociateDomain_fleetArn :: Lens.Lens' DisassociateDomain Prelude.Text
disassociateDomain_fleetArn = Lens.lens (\DisassociateDomain' {fleetArn} -> fleetArn) (\s@DisassociateDomain' {} a -> s {fleetArn = a} :: DisassociateDomain)

-- | The name of the domain.
disassociateDomain_domainName :: Lens.Lens' DisassociateDomain Prelude.Text
disassociateDomain_domainName = Lens.lens (\DisassociateDomain' {domainName} -> domainName) (\s@DisassociateDomain' {} a -> s {domainName = a} :: DisassociateDomain)

instance Core.AWSRequest DisassociateDomain where
  type
    AWSResponse DisassociateDomain =
      DisassociateDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateDomainResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateDomain where
  hashWithSalt salt' DisassociateDomain' {..} =
    salt' `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` fleetArn

instance Prelude.NFData DisassociateDomain where
  rnf DisassociateDomain' {..} =
    Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf domainName

instance Core.ToHeaders DisassociateDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DisassociateDomain where
  toJSON DisassociateDomain' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetArn" Core..= fleetArn),
            Prelude.Just ("DomainName" Core..= domainName)
          ]
      )

instance Core.ToPath DisassociateDomain where
  toPath = Prelude.const "/disassociateDomain"

instance Core.ToQuery DisassociateDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateDomainResponse' smart constructor.
data DisassociateDomainResponse = DisassociateDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateDomainResponse_httpStatus' - The response's http status code.
newDisassociateDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateDomainResponse
newDisassociateDomainResponse pHttpStatus_ =
  DisassociateDomainResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateDomainResponse_httpStatus :: Lens.Lens' DisassociateDomainResponse Prelude.Int
disassociateDomainResponse_httpStatus = Lens.lens (\DisassociateDomainResponse' {httpStatus} -> httpStatus) (\s@DisassociateDomainResponse' {} a -> s {httpStatus = a} :: DisassociateDomainResponse)

instance Prelude.NFData DisassociateDomainResponse where
  rnf DisassociateDomainResponse' {..} =
    Prelude.rnf httpStatus
