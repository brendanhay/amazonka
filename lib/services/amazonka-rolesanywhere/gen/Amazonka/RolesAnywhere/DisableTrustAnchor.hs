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
-- Module      : Amazonka.RolesAnywhere.DisableTrustAnchor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables a trust anchor. When disabled,
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- requests specifying this trust anchor are unauthorized.
--
-- __Required permissions:__ @rolesanywhere:DisableTrustAnchor@.
module Amazonka.RolesAnywhere.DisableTrustAnchor
  ( -- * Creating a Request
    DisableTrustAnchor (..),
    newDisableTrustAnchor,

    -- * Request Lenses
    disableTrustAnchor_trustAnchorId,

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

-- | /See:/ 'newDisableTrustAnchor' smart constructor.
data DisableTrustAnchor = DisableTrustAnchor'
  { -- | The unique identifier of the trust anchor.
    trustAnchorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableTrustAnchor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustAnchorId', 'disableTrustAnchor_trustAnchorId' - The unique identifier of the trust anchor.
newDisableTrustAnchor ::
  -- | 'trustAnchorId'
  Prelude.Text ->
  DisableTrustAnchor
newDisableTrustAnchor pTrustAnchorId_ =
  DisableTrustAnchor'
    { trustAnchorId =
        pTrustAnchorId_
    }

-- | The unique identifier of the trust anchor.
disableTrustAnchor_trustAnchorId :: Lens.Lens' DisableTrustAnchor Prelude.Text
disableTrustAnchor_trustAnchorId = Lens.lens (\DisableTrustAnchor' {trustAnchorId} -> trustAnchorId) (\s@DisableTrustAnchor' {} a -> s {trustAnchorId = a} :: DisableTrustAnchor)

instance Core.AWSRequest DisableTrustAnchor where
  type
    AWSResponse DisableTrustAnchor =
      TrustAnchorDetailResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable DisableTrustAnchor where
  hashWithSalt _salt DisableTrustAnchor' {..} =
    _salt `Prelude.hashWithSalt` trustAnchorId

instance Prelude.NFData DisableTrustAnchor where
  rnf DisableTrustAnchor' {..} =
    Prelude.rnf trustAnchorId

instance Data.ToHeaders DisableTrustAnchor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisableTrustAnchor where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DisableTrustAnchor where
  toPath DisableTrustAnchor' {..} =
    Prelude.mconcat
      [ "/trustanchor/",
        Data.toBS trustAnchorId,
        "/disable"
      ]

instance Data.ToQuery DisableTrustAnchor where
  toQuery = Prelude.const Prelude.mempty
