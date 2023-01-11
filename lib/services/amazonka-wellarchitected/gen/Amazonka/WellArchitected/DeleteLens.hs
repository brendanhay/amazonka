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
-- Module      : Amazonka.WellArchitected.DeleteLens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an existing lens.
--
-- Only the owner of a lens can delete it. After the lens is deleted,
-- Amazon Web Services accounts and IAM users that you shared the lens with
-- can continue to use it, but they will no longer be able to apply it to
-- new workloads.
--
-- __Disclaimer__
--
-- By sharing your custom lenses with other Amazon Web Services accounts,
-- you acknowledge that Amazon Web Services will make your custom lenses
-- available to those other accounts. Those other accounts may continue to
-- access and use your shared custom lenses even if you delete the custom
-- lenses from your own Amazon Web Services account or terminate your
-- Amazon Web Services account.
module Amazonka.WellArchitected.DeleteLens
  ( -- * Creating a Request
    DeleteLens (..),
    newDeleteLens,

    -- * Request Lenses
    deleteLens_lensAlias,
    deleteLens_clientRequestToken,
    deleteLens_lensStatus,

    -- * Destructuring the Response
    DeleteLensResponse (..),
    newDeleteLensResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newDeleteLens' smart constructor.
data DeleteLens = DeleteLens'
  { lensAlias :: Prelude.Text,
    clientRequestToken :: Prelude.Text,
    -- | The status of the lens to be deleted.
    lensStatus :: LensStatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLens' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lensAlias', 'deleteLens_lensAlias' - Undocumented member.
--
-- 'clientRequestToken', 'deleteLens_clientRequestToken' - Undocumented member.
--
-- 'lensStatus', 'deleteLens_lensStatus' - The status of the lens to be deleted.
newDeleteLens ::
  -- | 'lensAlias'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  -- | 'lensStatus'
  LensStatusType ->
  DeleteLens
newDeleteLens
  pLensAlias_
  pClientRequestToken_
  pLensStatus_ =
    DeleteLens'
      { lensAlias = pLensAlias_,
        clientRequestToken = pClientRequestToken_,
        lensStatus = pLensStatus_
      }

-- | Undocumented member.
deleteLens_lensAlias :: Lens.Lens' DeleteLens Prelude.Text
deleteLens_lensAlias = Lens.lens (\DeleteLens' {lensAlias} -> lensAlias) (\s@DeleteLens' {} a -> s {lensAlias = a} :: DeleteLens)

-- | Undocumented member.
deleteLens_clientRequestToken :: Lens.Lens' DeleteLens Prelude.Text
deleteLens_clientRequestToken = Lens.lens (\DeleteLens' {clientRequestToken} -> clientRequestToken) (\s@DeleteLens' {} a -> s {clientRequestToken = a} :: DeleteLens)

-- | The status of the lens to be deleted.
deleteLens_lensStatus :: Lens.Lens' DeleteLens LensStatusType
deleteLens_lensStatus = Lens.lens (\DeleteLens' {lensStatus} -> lensStatus) (\s@DeleteLens' {} a -> s {lensStatus = a} :: DeleteLens)

instance Core.AWSRequest DeleteLens where
  type AWSResponse DeleteLens = DeleteLensResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteLensResponse'

instance Prelude.Hashable DeleteLens where
  hashWithSalt _salt DeleteLens' {..} =
    _salt `Prelude.hashWithSalt` lensAlias
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` lensStatus

instance Prelude.NFData DeleteLens where
  rnf DeleteLens' {..} =
    Prelude.rnf lensAlias
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf lensStatus

instance Data.ToHeaders DeleteLens where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteLens where
  toPath DeleteLens' {..} =
    Prelude.mconcat ["/lenses/", Data.toBS lensAlias]

instance Data.ToQuery DeleteLens where
  toQuery DeleteLens' {..} =
    Prelude.mconcat
      [ "ClientRequestToken" Data.=: clientRequestToken,
        "LensStatus" Data.=: lensStatus
      ]

-- | /See:/ 'newDeleteLensResponse' smart constructor.
data DeleteLensResponse = DeleteLensResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLensResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLensResponse ::
  DeleteLensResponse
newDeleteLensResponse = DeleteLensResponse'

instance Prelude.NFData DeleteLensResponse where
  rnf _ = ()
