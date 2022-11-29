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
-- Module      : Amazonka.Transfer.DeleteHostKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the host key that\'s specified in the @HoskKeyId@ parameter.
module Amazonka.Transfer.DeleteHostKey
  ( -- * Creating a Request
    DeleteHostKey (..),
    newDeleteHostKey,

    -- * Request Lenses
    deleteHostKey_serverId,
    deleteHostKey_hostKeyId,

    -- * Destructuring the Response
    DeleteHostKeyResponse (..),
    newDeleteHostKeyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newDeleteHostKey' smart constructor.
data DeleteHostKey = DeleteHostKey'
  { -- | The identifier of the server that contains the host key that you are
    -- deleting.
    serverId :: Prelude.Text,
    -- | The identifier of the host key that you are deleting.
    hostKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHostKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverId', 'deleteHostKey_serverId' - The identifier of the server that contains the host key that you are
-- deleting.
--
-- 'hostKeyId', 'deleteHostKey_hostKeyId' - The identifier of the host key that you are deleting.
newDeleteHostKey ::
  -- | 'serverId'
  Prelude.Text ->
  -- | 'hostKeyId'
  Prelude.Text ->
  DeleteHostKey
newDeleteHostKey pServerId_ pHostKeyId_ =
  DeleteHostKey'
    { serverId = pServerId_,
      hostKeyId = pHostKeyId_
    }

-- | The identifier of the server that contains the host key that you are
-- deleting.
deleteHostKey_serverId :: Lens.Lens' DeleteHostKey Prelude.Text
deleteHostKey_serverId = Lens.lens (\DeleteHostKey' {serverId} -> serverId) (\s@DeleteHostKey' {} a -> s {serverId = a} :: DeleteHostKey)

-- | The identifier of the host key that you are deleting.
deleteHostKey_hostKeyId :: Lens.Lens' DeleteHostKey Prelude.Text
deleteHostKey_hostKeyId = Lens.lens (\DeleteHostKey' {hostKeyId} -> hostKeyId) (\s@DeleteHostKey' {} a -> s {hostKeyId = a} :: DeleteHostKey)

instance Core.AWSRequest DeleteHostKey where
  type
    AWSResponse DeleteHostKey =
      DeleteHostKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteHostKeyResponse'

instance Prelude.Hashable DeleteHostKey where
  hashWithSalt _salt DeleteHostKey' {..} =
    _salt `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` hostKeyId

instance Prelude.NFData DeleteHostKey where
  rnf DeleteHostKey' {..} =
    Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf hostKeyId

instance Core.ToHeaders DeleteHostKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TransferService.DeleteHostKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteHostKey where
  toJSON DeleteHostKey' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ServerId" Core..= serverId),
            Prelude.Just ("HostKeyId" Core..= hostKeyId)
          ]
      )

instance Core.ToPath DeleteHostKey where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteHostKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteHostKeyResponse' smart constructor.
data DeleteHostKeyResponse = DeleteHostKeyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHostKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteHostKeyResponse ::
  DeleteHostKeyResponse
newDeleteHostKeyResponse = DeleteHostKeyResponse'

instance Prelude.NFData DeleteHostKeyResponse where
  rnf _ = ()
