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
-- Module      : Amazonka.CloudFormation.ActivateOrganizationsAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activate trusted access with Organizations. With trusted access between
-- StackSets and Organizations activated, the management account has
-- permissions to create and manage StackSets for your organization.
module Amazonka.CloudFormation.ActivateOrganizationsAccess
  ( -- * Creating a Request
    ActivateOrganizationsAccess (..),
    newActivateOrganizationsAccess,

    -- * Destructuring the Response
    ActivateOrganizationsAccessResponse (..),
    newActivateOrganizationsAccessResponse,

    -- * Response Lenses
    activateOrganizationsAccessResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newActivateOrganizationsAccess' smart constructor.
data ActivateOrganizationsAccess = ActivateOrganizationsAccess'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateOrganizationsAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newActivateOrganizationsAccess ::
  ActivateOrganizationsAccess
newActivateOrganizationsAccess =
  ActivateOrganizationsAccess'

instance Core.AWSRequest ActivateOrganizationsAccess where
  type
    AWSResponse ActivateOrganizationsAccess =
      ActivateOrganizationsAccessResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ActivateOrganizationsAccessResult"
      ( \s h x ->
          ActivateOrganizationsAccessResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ActivateOrganizationsAccess where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData ActivateOrganizationsAccess where
  rnf _ = ()

instance Data.ToHeaders ActivateOrganizationsAccess where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ActivateOrganizationsAccess where
  toPath = Prelude.const "/"

instance Data.ToQuery ActivateOrganizationsAccess where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ( "ActivateOrganizationsAccess" ::
                          Prelude.ByteString
                      ),
            "Version"
              Data.=: ("2010-05-15" :: Prelude.ByteString)
          ]
      )

-- | /See:/ 'newActivateOrganizationsAccessResponse' smart constructor.
data ActivateOrganizationsAccessResponse = ActivateOrganizationsAccessResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateOrganizationsAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'activateOrganizationsAccessResponse_httpStatus' - The response's http status code.
newActivateOrganizationsAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ActivateOrganizationsAccessResponse
newActivateOrganizationsAccessResponse pHttpStatus_ =
  ActivateOrganizationsAccessResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
activateOrganizationsAccessResponse_httpStatus :: Lens.Lens' ActivateOrganizationsAccessResponse Prelude.Int
activateOrganizationsAccessResponse_httpStatus = Lens.lens (\ActivateOrganizationsAccessResponse' {httpStatus} -> httpStatus) (\s@ActivateOrganizationsAccessResponse' {} a -> s {httpStatus = a} :: ActivateOrganizationsAccessResponse)

instance
  Prelude.NFData
    ActivateOrganizationsAccessResponse
  where
  rnf ActivateOrganizationsAccessResponse' {..} =
    Prelude.rnf httpStatus
