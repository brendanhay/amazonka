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
-- Module      : Amazonka.CloudFront.DeletePublicKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a public key you previously added to CloudFront.
module Amazonka.CloudFront.DeletePublicKey
  ( -- * Creating a Request
    DeletePublicKey (..),
    newDeletePublicKey,

    -- * Request Lenses
    deletePublicKey_ifMatch,
    deletePublicKey_id,

    -- * Destructuring the Response
    DeletePublicKeyResponse (..),
    newDeletePublicKeyResponse,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePublicKey' smart constructor.
data DeletePublicKey = DeletePublicKey'
  { -- | The value of the @ETag@ header that you received when retrieving the
    -- public key identity to delete. For example: @E2QWRUHAPOMQZL@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | The ID of the public key you want to remove from CloudFront.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePublicKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'deletePublicKey_ifMatch' - The value of the @ETag@ header that you received when retrieving the
-- public key identity to delete. For example: @E2QWRUHAPOMQZL@.
--
-- 'id', 'deletePublicKey_id' - The ID of the public key you want to remove from CloudFront.
newDeletePublicKey ::
  -- | 'id'
  Prelude.Text ->
  DeletePublicKey
newDeletePublicKey pId_ =
  DeletePublicKey'
    { ifMatch = Prelude.Nothing,
      id = pId_
    }

-- | The value of the @ETag@ header that you received when retrieving the
-- public key identity to delete. For example: @E2QWRUHAPOMQZL@.
deletePublicKey_ifMatch :: Lens.Lens' DeletePublicKey (Prelude.Maybe Prelude.Text)
deletePublicKey_ifMatch = Lens.lens (\DeletePublicKey' {ifMatch} -> ifMatch) (\s@DeletePublicKey' {} a -> s {ifMatch = a} :: DeletePublicKey)

-- | The ID of the public key you want to remove from CloudFront.
deletePublicKey_id :: Lens.Lens' DeletePublicKey Prelude.Text
deletePublicKey_id = Lens.lens (\DeletePublicKey' {id} -> id) (\s@DeletePublicKey' {} a -> s {id = a} :: DeletePublicKey)

instance Core.AWSRequest DeletePublicKey where
  type
    AWSResponse DeletePublicKey =
      DeletePublicKeyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeletePublicKeyResponse'

instance Prelude.Hashable DeletePublicKey where
  hashWithSalt _salt DeletePublicKey' {..} =
    _salt `Prelude.hashWithSalt` ifMatch
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeletePublicKey where
  rnf DeletePublicKey' {..} =
    Prelude.rnf ifMatch `Prelude.seq` Prelude.rnf id

instance Core.ToHeaders DeletePublicKey where
  toHeaders DeletePublicKey' {..} =
    Prelude.mconcat ["If-Match" Core.=# ifMatch]

instance Core.ToPath DeletePublicKey where
  toPath DeletePublicKey' {..} =
    Prelude.mconcat
      ["/2020-05-31/public-key/", Core.toBS id]

instance Core.ToQuery DeletePublicKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePublicKeyResponse' smart constructor.
data DeletePublicKeyResponse = DeletePublicKeyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePublicKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePublicKeyResponse ::
  DeletePublicKeyResponse
newDeletePublicKeyResponse = DeletePublicKeyResponse'

instance Prelude.NFData DeletePublicKeyResponse where
  rnf _ = ()
