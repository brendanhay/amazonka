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
-- Module      : Amazonka.RolesAnywhere.DisableCrl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables a certificate revocation list (CRL).
--
-- __Required permissions:__ @rolesanywhere:DisableCrl@.
module Amazonka.RolesAnywhere.DisableCrl
  ( -- * Creating a Request
    DisableCrl (..),
    newDisableCrl,

    -- * Request Lenses
    disableCrl_crlId,

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

-- | /See:/ 'newDisableCrl' smart constructor.
data DisableCrl = DisableCrl'
  { -- | The unique identifier of the certificate revocation list (CRL).
    crlId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableCrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crlId', 'disableCrl_crlId' - The unique identifier of the certificate revocation list (CRL).
newDisableCrl ::
  -- | 'crlId'
  Prelude.Text ->
  DisableCrl
newDisableCrl pCrlId_ = DisableCrl' {crlId = pCrlId_}

-- | The unique identifier of the certificate revocation list (CRL).
disableCrl_crlId :: Lens.Lens' DisableCrl Prelude.Text
disableCrl_crlId = Lens.lens (\DisableCrl' {crlId} -> crlId) (\s@DisableCrl' {} a -> s {crlId = a} :: DisableCrl)

instance Core.AWSRequest DisableCrl where
  type AWSResponse DisableCrl = CrlDetailResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable DisableCrl where
  hashWithSalt _salt DisableCrl' {..} =
    _salt `Prelude.hashWithSalt` crlId

instance Prelude.NFData DisableCrl where
  rnf DisableCrl' {..} = Prelude.rnf crlId

instance Data.ToHeaders DisableCrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisableCrl where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DisableCrl where
  toPath DisableCrl' {..} =
    Prelude.mconcat
      ["/crl/", Data.toBS crlId, "/disable"]

instance Data.ToQuery DisableCrl where
  toQuery = Prelude.const Prelude.mempty
