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
-- Module      : Amazonka.RolesAnywhere.EnableTrustAnchor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a trust anchor. When enabled, certificates in the trust anchor
-- chain are authorized for trust validation.
--
-- __Required permissions:__ @rolesanywhere:EnableTrustAnchor@.
module Amazonka.RolesAnywhere.EnableTrustAnchor
  ( -- * Creating a Request
    EnableTrustAnchor (..),
    newEnableTrustAnchor,

    -- * Request Lenses
    enableTrustAnchor_trustAnchorId,

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

-- | /See:/ 'newEnableTrustAnchor' smart constructor.
data EnableTrustAnchor = EnableTrustAnchor'
  { -- | The unique identifier of the trust anchor.
    trustAnchorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableTrustAnchor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustAnchorId', 'enableTrustAnchor_trustAnchorId' - The unique identifier of the trust anchor.
newEnableTrustAnchor ::
  -- | 'trustAnchorId'
  Prelude.Text ->
  EnableTrustAnchor
newEnableTrustAnchor pTrustAnchorId_ =
  EnableTrustAnchor' {trustAnchorId = pTrustAnchorId_}

-- | The unique identifier of the trust anchor.
enableTrustAnchor_trustAnchorId :: Lens.Lens' EnableTrustAnchor Prelude.Text
enableTrustAnchor_trustAnchorId = Lens.lens (\EnableTrustAnchor' {trustAnchorId} -> trustAnchorId) (\s@EnableTrustAnchor' {} a -> s {trustAnchorId = a} :: EnableTrustAnchor)

instance Core.AWSRequest EnableTrustAnchor where
  type
    AWSResponse EnableTrustAnchor =
      TrustAnchorDetailResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable EnableTrustAnchor where
  hashWithSalt _salt EnableTrustAnchor' {..} =
    _salt `Prelude.hashWithSalt` trustAnchorId

instance Prelude.NFData EnableTrustAnchor where
  rnf EnableTrustAnchor' {..} =
    Prelude.rnf trustAnchorId

instance Data.ToHeaders EnableTrustAnchor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EnableTrustAnchor where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath EnableTrustAnchor where
  toPath EnableTrustAnchor' {..} =
    Prelude.mconcat
      ["/trustanchor/", Data.toBS trustAnchorId, "/enable"]

instance Data.ToQuery EnableTrustAnchor where
  toQuery = Prelude.const Prelude.mempty
