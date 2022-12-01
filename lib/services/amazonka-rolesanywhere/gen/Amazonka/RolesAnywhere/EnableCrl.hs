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
-- Module      : Amazonka.RolesAnywhere.EnableCrl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a certificate revocation list (CRL). When enabled, certificates
-- stored in the CRL are unauthorized to receive session credentials.
--
-- __Required permissions:__ @rolesanywhere:EnableCrl@.
module Amazonka.RolesAnywhere.EnableCrl
  ( -- * Creating a Request
    EnableCrl (..),
    newEnableCrl,

    -- * Request Lenses
    enableCrl_crlId,

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

-- | /See:/ 'newEnableCrl' smart constructor.
data EnableCrl = EnableCrl'
  { -- | The unique identifier of the certificate revocation list (CRL).
    crlId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableCrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crlId', 'enableCrl_crlId' - The unique identifier of the certificate revocation list (CRL).
newEnableCrl ::
  -- | 'crlId'
  Prelude.Text ->
  EnableCrl
newEnableCrl pCrlId_ = EnableCrl' {crlId = pCrlId_}

-- | The unique identifier of the certificate revocation list (CRL).
enableCrl_crlId :: Lens.Lens' EnableCrl Prelude.Text
enableCrl_crlId = Lens.lens (\EnableCrl' {crlId} -> crlId) (\s@EnableCrl' {} a -> s {crlId = a} :: EnableCrl)

instance Core.AWSRequest EnableCrl where
  type AWSResponse EnableCrl = CrlDetailResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable EnableCrl where
  hashWithSalt _salt EnableCrl' {..} =
    _salt `Prelude.hashWithSalt` crlId

instance Prelude.NFData EnableCrl where
  rnf EnableCrl' {..} = Prelude.rnf crlId

instance Core.ToHeaders EnableCrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON EnableCrl where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath EnableCrl where
  toPath EnableCrl' {..} =
    Prelude.mconcat
      ["/crl/", Core.toBS crlId, "/enable"]

instance Core.ToQuery EnableCrl where
  toQuery = Prelude.const Prelude.mempty
