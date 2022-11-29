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
-- Module      : Amazonka.RolesAnywhere.DeleteCrl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a certificate revocation list (CRL).
--
-- __Required permissions:__ @rolesanywhere:DeleteCrl@.
module Amazonka.RolesAnywhere.DeleteCrl
  ( -- * Creating a Request
    DeleteCrl (..),
    newDeleteCrl,

    -- * Request Lenses
    deleteCrl_crlId,

    -- * Destructuring the Response
    CrlDetailResponse (..),
    newCrlDetailResponse,

    -- * Response Lenses
    crlDetailResponse_crl,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RolesAnywhere.Types

-- | /See:/ 'newDeleteCrl' smart constructor.
data DeleteCrl = DeleteCrl'
  { -- | The unique identifier of the certificate revocation list (CRL).
    crlId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crlId', 'deleteCrl_crlId' - The unique identifier of the certificate revocation list (CRL).
newDeleteCrl ::
  -- | 'crlId'
  Prelude.Text ->
  DeleteCrl
newDeleteCrl pCrlId_ = DeleteCrl' {crlId = pCrlId_}

-- | The unique identifier of the certificate revocation list (CRL).
deleteCrl_crlId :: Lens.Lens' DeleteCrl Prelude.Text
deleteCrl_crlId = Lens.lens (\DeleteCrl' {crlId} -> crlId) (\s@DeleteCrl' {} a -> s {crlId = a} :: DeleteCrl)

instance Core.AWSRequest DeleteCrl where
  type AWSResponse DeleteCrl = CrlDetailResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable DeleteCrl where
  hashWithSalt _salt DeleteCrl' {..} =
    _salt `Prelude.hashWithSalt` crlId

instance Prelude.NFData DeleteCrl where
  rnf DeleteCrl' {..} = Prelude.rnf crlId

instance Core.ToHeaders DeleteCrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteCrl where
  toPath DeleteCrl' {..} =
    Prelude.mconcat ["/crl/", Core.toBS crlId]

instance Core.ToQuery DeleteCrl where
  toQuery = Prelude.const Prelude.mempty
