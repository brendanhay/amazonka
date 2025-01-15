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
-- Module      : Amazonka.RolesAnywhere.UpdateTrustAnchor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the trust anchor.You establish trust between IAM Roles Anywhere
-- and your certificate authority (CA) by configuring a trust anchor. A
-- Trust Anchor is defined either as a reference to a AWS Certificate
-- Manager Private Certificate Authority (ACM PCA), or by uploading a
-- Certificate Authority (CA) certificate. Your AWS workloads can
-- authenticate with the trust anchor using certificates issued by the
-- trusted Certificate Authority (CA) in exchange for temporary AWS
-- credentials.
--
-- __Required permissions:__ @rolesanywhere:UpdateTrustAnchor@.
module Amazonka.RolesAnywhere.UpdateTrustAnchor
  ( -- * Creating a Request
    UpdateTrustAnchor (..),
    newUpdateTrustAnchor,

    -- * Request Lenses
    updateTrustAnchor_name,
    updateTrustAnchor_source,
    updateTrustAnchor_trustAnchorId,

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

-- | /See:/ 'newUpdateTrustAnchor' smart constructor.
data UpdateTrustAnchor = UpdateTrustAnchor'
  { -- | The name of the trust anchor.
    name :: Prelude.Maybe Prelude.Text,
    -- | The trust anchor type and its related certificate data.
    source :: Prelude.Maybe Source,
    -- | The unique identifier of the trust anchor.
    trustAnchorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTrustAnchor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateTrustAnchor_name' - The name of the trust anchor.
--
-- 'source', 'updateTrustAnchor_source' - The trust anchor type and its related certificate data.
--
-- 'trustAnchorId', 'updateTrustAnchor_trustAnchorId' - The unique identifier of the trust anchor.
newUpdateTrustAnchor ::
  -- | 'trustAnchorId'
  Prelude.Text ->
  UpdateTrustAnchor
newUpdateTrustAnchor pTrustAnchorId_ =
  UpdateTrustAnchor'
    { name = Prelude.Nothing,
      source = Prelude.Nothing,
      trustAnchorId = pTrustAnchorId_
    }

-- | The name of the trust anchor.
updateTrustAnchor_name :: Lens.Lens' UpdateTrustAnchor (Prelude.Maybe Prelude.Text)
updateTrustAnchor_name = Lens.lens (\UpdateTrustAnchor' {name} -> name) (\s@UpdateTrustAnchor' {} a -> s {name = a} :: UpdateTrustAnchor)

-- | The trust anchor type and its related certificate data.
updateTrustAnchor_source :: Lens.Lens' UpdateTrustAnchor (Prelude.Maybe Source)
updateTrustAnchor_source = Lens.lens (\UpdateTrustAnchor' {source} -> source) (\s@UpdateTrustAnchor' {} a -> s {source = a} :: UpdateTrustAnchor)

-- | The unique identifier of the trust anchor.
updateTrustAnchor_trustAnchorId :: Lens.Lens' UpdateTrustAnchor Prelude.Text
updateTrustAnchor_trustAnchorId = Lens.lens (\UpdateTrustAnchor' {trustAnchorId} -> trustAnchorId) (\s@UpdateTrustAnchor' {} a -> s {trustAnchorId = a} :: UpdateTrustAnchor)

instance Core.AWSRequest UpdateTrustAnchor where
  type
    AWSResponse UpdateTrustAnchor =
      TrustAnchorDetailResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateTrustAnchor where
  hashWithSalt _salt UpdateTrustAnchor' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` trustAnchorId

instance Prelude.NFData UpdateTrustAnchor where
  rnf UpdateTrustAnchor' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf source `Prelude.seq`
        Prelude.rnf trustAnchorId

instance Data.ToHeaders UpdateTrustAnchor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTrustAnchor where
  toJSON UpdateTrustAnchor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("source" Data..=) Prelude.<$> source
          ]
      )

instance Data.ToPath UpdateTrustAnchor where
  toPath UpdateTrustAnchor' {..} =
    Prelude.mconcat
      ["/trustanchor/", Data.toBS trustAnchorId]

instance Data.ToQuery UpdateTrustAnchor where
  toQuery = Prelude.const Prelude.mempty
