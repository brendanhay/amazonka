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
-- Module      : Amazonka.RolesAnywhere.UpdateCrl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the certificate revocation list (CRL). A CRL is a list of
-- certificates that have been revoked by the issuing certificate authority
-- (CA). IAM Roles Anywhere validates against the CRL before issuing
-- credentials.
--
-- __Required permissions:__ @rolesanywhere:UpdateCrl@.
module Amazonka.RolesAnywhere.UpdateCrl
  ( -- * Creating a Request
    UpdateCrl (..),
    newUpdateCrl,

    -- * Request Lenses
    updateCrl_crlData,
    updateCrl_name,
    updateCrl_crlId,

    -- * Destructuring the Response
    CrlDetailResponse (..),
    newCrlDetailResponse,

    -- * Response Lenses
    crlDetailResponse_crl,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RolesAnywhere.Types

-- | /See:/ 'newUpdateCrl' smart constructor.
data UpdateCrl = UpdateCrl'
  { -- | The x509 v3 specified certificate revocation list (CRL).
    crlData :: Prelude.Maybe Data.Base64,
    -- | The name of the Crl.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the certificate revocation list (CRL).
    crlId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crlData', 'updateCrl_crlData' - The x509 v3 specified certificate revocation list (CRL).--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'name', 'updateCrl_name' - The name of the Crl.
--
-- 'crlId', 'updateCrl_crlId' - The unique identifier of the certificate revocation list (CRL).
newUpdateCrl ::
  -- | 'crlId'
  Prelude.Text ->
  UpdateCrl
newUpdateCrl pCrlId_ =
  UpdateCrl'
    { crlData = Prelude.Nothing,
      name = Prelude.Nothing,
      crlId = pCrlId_
    }

-- | The x509 v3 specified certificate revocation list (CRL).--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
updateCrl_crlData :: Lens.Lens' UpdateCrl (Prelude.Maybe Prelude.ByteString)
updateCrl_crlData = Lens.lens (\UpdateCrl' {crlData} -> crlData) (\s@UpdateCrl' {} a -> s {crlData = a} :: UpdateCrl) Prelude.. Lens.mapping Data._Base64

-- | The name of the Crl.
updateCrl_name :: Lens.Lens' UpdateCrl (Prelude.Maybe Prelude.Text)
updateCrl_name = Lens.lens (\UpdateCrl' {name} -> name) (\s@UpdateCrl' {} a -> s {name = a} :: UpdateCrl)

-- | The unique identifier of the certificate revocation list (CRL).
updateCrl_crlId :: Lens.Lens' UpdateCrl Prelude.Text
updateCrl_crlId = Lens.lens (\UpdateCrl' {crlId} -> crlId) (\s@UpdateCrl' {} a -> s {crlId = a} :: UpdateCrl)

instance Core.AWSRequest UpdateCrl where
  type AWSResponse UpdateCrl = CrlDetailResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateCrl where
  hashWithSalt _salt UpdateCrl' {..} =
    _salt
      `Prelude.hashWithSalt` crlData
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` crlId

instance Prelude.NFData UpdateCrl where
  rnf UpdateCrl' {..} =
    Prelude.rnf crlData
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf crlId

instance Data.ToHeaders UpdateCrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCrl where
  toJSON UpdateCrl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("crlData" Data..=) Prelude.<$> crlData,
            ("name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateCrl where
  toPath UpdateCrl' {..} =
    Prelude.mconcat ["/crl/", Data.toBS crlId]

instance Data.ToQuery UpdateCrl where
  toQuery = Prelude.const Prelude.mempty
