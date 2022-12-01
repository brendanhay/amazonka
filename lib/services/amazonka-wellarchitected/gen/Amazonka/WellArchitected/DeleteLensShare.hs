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
-- Module      : Amazonka.WellArchitected.DeleteLensShare
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a lens share.
--
-- After the lens share is deleted, Amazon Web Services accounts, IAM
-- users, organizations, and organizational units (OUs) that you shared the
-- lens with can continue to use it, but they will no longer be able to
-- apply it to new workloads.
--
-- __Disclaimer__
--
-- By sharing your custom lenses with other Amazon Web Services accounts,
-- you acknowledge that Amazon Web Services will make your custom lenses
-- available to those other accounts. Those other accounts may continue to
-- access and use your shared custom lenses even if you delete the custom
-- lenses from your own Amazon Web Services account or terminate your
-- Amazon Web Services account.
module Amazonka.WellArchitected.DeleteLensShare
  ( -- * Creating a Request
    DeleteLensShare (..),
    newDeleteLensShare,

    -- * Request Lenses
    deleteLensShare_shareId,
    deleteLensShare_lensAlias,
    deleteLensShare_clientRequestToken,

    -- * Destructuring the Response
    DeleteLensShareResponse (..),
    newDeleteLensShareResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newDeleteLensShare' smart constructor.
data DeleteLensShare = DeleteLensShare'
  { shareId :: Prelude.Text,
    lensAlias :: Prelude.Text,
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLensShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shareId', 'deleteLensShare_shareId' - Undocumented member.
--
-- 'lensAlias', 'deleteLensShare_lensAlias' - Undocumented member.
--
-- 'clientRequestToken', 'deleteLensShare_clientRequestToken' - Undocumented member.
newDeleteLensShare ::
  -- | 'shareId'
  Prelude.Text ->
  -- | 'lensAlias'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  DeleteLensShare
newDeleteLensShare
  pShareId_
  pLensAlias_
  pClientRequestToken_ =
    DeleteLensShare'
      { shareId = pShareId_,
        lensAlias = pLensAlias_,
        clientRequestToken = pClientRequestToken_
      }

-- | Undocumented member.
deleteLensShare_shareId :: Lens.Lens' DeleteLensShare Prelude.Text
deleteLensShare_shareId = Lens.lens (\DeleteLensShare' {shareId} -> shareId) (\s@DeleteLensShare' {} a -> s {shareId = a} :: DeleteLensShare)

-- | Undocumented member.
deleteLensShare_lensAlias :: Lens.Lens' DeleteLensShare Prelude.Text
deleteLensShare_lensAlias = Lens.lens (\DeleteLensShare' {lensAlias} -> lensAlias) (\s@DeleteLensShare' {} a -> s {lensAlias = a} :: DeleteLensShare)

-- | Undocumented member.
deleteLensShare_clientRequestToken :: Lens.Lens' DeleteLensShare Prelude.Text
deleteLensShare_clientRequestToken = Lens.lens (\DeleteLensShare' {clientRequestToken} -> clientRequestToken) (\s@DeleteLensShare' {} a -> s {clientRequestToken = a} :: DeleteLensShare)

instance Core.AWSRequest DeleteLensShare where
  type
    AWSResponse DeleteLensShare =
      DeleteLensShareResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteLensShareResponse'

instance Prelude.Hashable DeleteLensShare where
  hashWithSalt _salt DeleteLensShare' {..} =
    _salt `Prelude.hashWithSalt` shareId
      `Prelude.hashWithSalt` lensAlias
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData DeleteLensShare where
  rnf DeleteLensShare' {..} =
    Prelude.rnf shareId
      `Prelude.seq` Prelude.rnf lensAlias
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Core.ToHeaders DeleteLensShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteLensShare where
  toPath DeleteLensShare' {..} =
    Prelude.mconcat
      [ "/lenses/",
        Core.toBS lensAlias,
        "/shares/",
        Core.toBS shareId
      ]

instance Core.ToQuery DeleteLensShare where
  toQuery DeleteLensShare' {..} =
    Prelude.mconcat
      ["ClientRequestToken" Core.=: clientRequestToken]

-- | /See:/ 'newDeleteLensShareResponse' smart constructor.
data DeleteLensShareResponse = DeleteLensShareResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLensShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLensShareResponse ::
  DeleteLensShareResponse
newDeleteLensShareResponse = DeleteLensShareResponse'

instance Prelude.NFData DeleteLensShareResponse where
  rnf _ = ()
