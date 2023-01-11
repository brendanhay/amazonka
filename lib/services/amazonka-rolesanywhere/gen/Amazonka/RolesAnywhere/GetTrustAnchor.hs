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
-- Module      : Amazonka.RolesAnywhere.GetTrustAnchor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a trust anchor.
--
-- __Required permissions:__ @rolesanywhere:GetTrustAnchor@.
module Amazonka.RolesAnywhere.GetTrustAnchor
  ( -- * Creating a Request
    GetTrustAnchor (..),
    newGetTrustAnchor,

    -- * Request Lenses
    getTrustAnchor_trustAnchorId,

    -- * Destructuring the Response
    TrustAnchorDetailResponse (..),
    newTrustAnchorDetailResponse,

    -- * Response Lenses
    trustAnchorDetailResponse_trustAnchor,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RolesAnywhere.Types

-- | /See:/ 'newGetTrustAnchor' smart constructor.
data GetTrustAnchor = GetTrustAnchor'
  { -- | The unique identifier of the trust anchor.
    trustAnchorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTrustAnchor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustAnchorId', 'getTrustAnchor_trustAnchorId' - The unique identifier of the trust anchor.
newGetTrustAnchor ::
  -- | 'trustAnchorId'
  Prelude.Text ->
  GetTrustAnchor
newGetTrustAnchor pTrustAnchorId_ =
  GetTrustAnchor' {trustAnchorId = pTrustAnchorId_}

-- | The unique identifier of the trust anchor.
getTrustAnchor_trustAnchorId :: Lens.Lens' GetTrustAnchor Prelude.Text
getTrustAnchor_trustAnchorId = Lens.lens (\GetTrustAnchor' {trustAnchorId} -> trustAnchorId) (\s@GetTrustAnchor' {} a -> s {trustAnchorId = a} :: GetTrustAnchor)

instance Core.AWSRequest GetTrustAnchor where
  type
    AWSResponse GetTrustAnchor =
      TrustAnchorDetailResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetTrustAnchor where
  hashWithSalt _salt GetTrustAnchor' {..} =
    _salt `Prelude.hashWithSalt` trustAnchorId

instance Prelude.NFData GetTrustAnchor where
  rnf GetTrustAnchor' {..} = Prelude.rnf trustAnchorId

instance Data.ToHeaders GetTrustAnchor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetTrustAnchor where
  toPath GetTrustAnchor' {..} =
    Prelude.mconcat
      ["/trustanchor/", Data.toBS trustAnchorId]

instance Data.ToQuery GetTrustAnchor where
  toQuery = Prelude.const Prelude.mempty
