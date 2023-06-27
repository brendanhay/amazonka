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
-- Module      : Amazonka.CloudFormation.DeactivateOrganizationsAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates trusted access with Organizations. If trusted access is
-- deactivated, the management account does not have permissions to create
-- and manage service-managed StackSets for your organization.
module Amazonka.CloudFormation.DeactivateOrganizationsAccess
  ( -- * Creating a Request
    DeactivateOrganizationsAccess (..),
    newDeactivateOrganizationsAccess,

    -- * Destructuring the Response
    DeactivateOrganizationsAccessResponse (..),
    newDeactivateOrganizationsAccessResponse,

    -- * Response Lenses
    deactivateOrganizationsAccessResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeactivateOrganizationsAccess' smart constructor.
data DeactivateOrganizationsAccess = DeactivateOrganizationsAccess'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeactivateOrganizationsAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeactivateOrganizationsAccess ::
  DeactivateOrganizationsAccess
newDeactivateOrganizationsAccess =
  DeactivateOrganizationsAccess'

instance
  Core.AWSRequest
    DeactivateOrganizationsAccess
  where
  type
    AWSResponse DeactivateOrganizationsAccess =
      DeactivateOrganizationsAccessResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeactivateOrganizationsAccessResult"
      ( \s h x ->
          DeactivateOrganizationsAccessResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeactivateOrganizationsAccess
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DeactivateOrganizationsAccess where
  rnf _ = ()

instance Data.ToHeaders DeactivateOrganizationsAccess where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeactivateOrganizationsAccess where
  toPath = Prelude.const "/"

instance Data.ToQuery DeactivateOrganizationsAccess where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ( "DeactivateOrganizationsAccess" ::
                          Prelude.ByteString
                      ),
            "Version"
              Data.=: ("2010-05-15" :: Prelude.ByteString)
          ]
      )

-- | /See:/ 'newDeactivateOrganizationsAccessResponse' smart constructor.
data DeactivateOrganizationsAccessResponse = DeactivateOrganizationsAccessResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeactivateOrganizationsAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deactivateOrganizationsAccessResponse_httpStatus' - The response's http status code.
newDeactivateOrganizationsAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeactivateOrganizationsAccessResponse
newDeactivateOrganizationsAccessResponse pHttpStatus_ =
  DeactivateOrganizationsAccessResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deactivateOrganizationsAccessResponse_httpStatus :: Lens.Lens' DeactivateOrganizationsAccessResponse Prelude.Int
deactivateOrganizationsAccessResponse_httpStatus = Lens.lens (\DeactivateOrganizationsAccessResponse' {httpStatus} -> httpStatus) (\s@DeactivateOrganizationsAccessResponse' {} a -> s {httpStatus = a} :: DeactivateOrganizationsAccessResponse)

instance
  Prelude.NFData
    DeactivateOrganizationsAccessResponse
  where
  rnf DeactivateOrganizationsAccessResponse' {..} =
    Prelude.rnf httpStatus
