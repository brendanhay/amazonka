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
-- Module      : Amazonka.WorkLink.RestoreDomainAccess
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves a domain to ACTIVE status if it was in the INACTIVE status.
module Amazonka.WorkLink.RestoreDomainAccess
  ( -- * Creating a Request
    RestoreDomainAccess (..),
    newRestoreDomainAccess,

    -- * Request Lenses
    restoreDomainAccess_fleetArn,
    restoreDomainAccess_domainName,

    -- * Destructuring the Response
    RestoreDomainAccessResponse (..),
    newRestoreDomainAccessResponse,

    -- * Response Lenses
    restoreDomainAccessResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newRestoreDomainAccess' smart constructor.
data RestoreDomainAccess = RestoreDomainAccess'
  { -- | The ARN of the fleet.
    fleetArn :: Prelude.Text,
    -- | The name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDomainAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'restoreDomainAccess_fleetArn' - The ARN of the fleet.
--
-- 'domainName', 'restoreDomainAccess_domainName' - The name of the domain.
newRestoreDomainAccess ::
  -- | 'fleetArn'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  RestoreDomainAccess
newRestoreDomainAccess pFleetArn_ pDomainName_ =
  RestoreDomainAccess'
    { fleetArn = pFleetArn_,
      domainName = pDomainName_
    }

-- | The ARN of the fleet.
restoreDomainAccess_fleetArn :: Lens.Lens' RestoreDomainAccess Prelude.Text
restoreDomainAccess_fleetArn = Lens.lens (\RestoreDomainAccess' {fleetArn} -> fleetArn) (\s@RestoreDomainAccess' {} a -> s {fleetArn = a} :: RestoreDomainAccess)

-- | The name of the domain.
restoreDomainAccess_domainName :: Lens.Lens' RestoreDomainAccess Prelude.Text
restoreDomainAccess_domainName = Lens.lens (\RestoreDomainAccess' {domainName} -> domainName) (\s@RestoreDomainAccess' {} a -> s {domainName = a} :: RestoreDomainAccess)

instance Core.AWSRequest RestoreDomainAccess where
  type
    AWSResponse RestoreDomainAccess =
      RestoreDomainAccessResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RestoreDomainAccessResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreDomainAccess

instance Prelude.NFData RestoreDomainAccess

instance Core.ToHeaders RestoreDomainAccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RestoreDomainAccess where
  toJSON RestoreDomainAccess' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetArn" Core..= fleetArn),
            Prelude.Just ("DomainName" Core..= domainName)
          ]
      )

instance Core.ToPath RestoreDomainAccess where
  toPath = Prelude.const "/restoreDomainAccess"

instance Core.ToQuery RestoreDomainAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRestoreDomainAccessResponse' smart constructor.
data RestoreDomainAccessResponse = RestoreDomainAccessResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDomainAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'restoreDomainAccessResponse_httpStatus' - The response's http status code.
newRestoreDomainAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreDomainAccessResponse
newRestoreDomainAccessResponse pHttpStatus_ =
  RestoreDomainAccessResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
restoreDomainAccessResponse_httpStatus :: Lens.Lens' RestoreDomainAccessResponse Prelude.Int
restoreDomainAccessResponse_httpStatus = Lens.lens (\RestoreDomainAccessResponse' {httpStatus} -> httpStatus) (\s@RestoreDomainAccessResponse' {} a -> s {httpStatus = a} :: RestoreDomainAccessResponse)

instance Prelude.NFData RestoreDomainAccessResponse
