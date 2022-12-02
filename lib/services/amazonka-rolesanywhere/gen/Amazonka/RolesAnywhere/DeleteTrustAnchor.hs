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
-- Module      : Amazonka.RolesAnywhere.DeleteTrustAnchor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a trust anchor.
--
-- __Required permissions:__ @rolesanywhere:DeleteTrustAnchor@.
module Amazonka.RolesAnywhere.DeleteTrustAnchor
  ( -- * Creating a Request
    DeleteTrustAnchor (..),
    newDeleteTrustAnchor,

    -- * Request Lenses
    deleteTrustAnchor_trustAnchorId,

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

-- | /See:/ 'newDeleteTrustAnchor' smart constructor.
data DeleteTrustAnchor = DeleteTrustAnchor'
  { -- | The unique identifier of the trust anchor.
    trustAnchorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrustAnchor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustAnchorId', 'deleteTrustAnchor_trustAnchorId' - The unique identifier of the trust anchor.
newDeleteTrustAnchor ::
  -- | 'trustAnchorId'
  Prelude.Text ->
  DeleteTrustAnchor
newDeleteTrustAnchor pTrustAnchorId_ =
  DeleteTrustAnchor' {trustAnchorId = pTrustAnchorId_}

-- | The unique identifier of the trust anchor.
deleteTrustAnchor_trustAnchorId :: Lens.Lens' DeleteTrustAnchor Prelude.Text
deleteTrustAnchor_trustAnchorId = Lens.lens (\DeleteTrustAnchor' {trustAnchorId} -> trustAnchorId) (\s@DeleteTrustAnchor' {} a -> s {trustAnchorId = a} :: DeleteTrustAnchor)

instance Core.AWSRequest DeleteTrustAnchor where
  type
    AWSResponse DeleteTrustAnchor =
      TrustAnchorDetailResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable DeleteTrustAnchor where
  hashWithSalt _salt DeleteTrustAnchor' {..} =
    _salt `Prelude.hashWithSalt` trustAnchorId

instance Prelude.NFData DeleteTrustAnchor where
  rnf DeleteTrustAnchor' {..} =
    Prelude.rnf trustAnchorId

instance Data.ToHeaders DeleteTrustAnchor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteTrustAnchor where
  toPath DeleteTrustAnchor' {..} =
    Prelude.mconcat
      ["/trustanchor/", Data.toBS trustAnchorId]

instance Data.ToQuery DeleteTrustAnchor where
  toQuery = Prelude.const Prelude.mempty
