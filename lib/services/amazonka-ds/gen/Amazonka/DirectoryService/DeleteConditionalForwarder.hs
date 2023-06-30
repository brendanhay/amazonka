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
-- Module      : Amazonka.DirectoryService.DeleteConditionalForwarder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a conditional forwarder that has been set up for your Amazon Web
-- Services directory.
module Amazonka.DirectoryService.DeleteConditionalForwarder
  ( -- * Creating a Request
    DeleteConditionalForwarder (..),
    newDeleteConditionalForwarder,

    -- * Request Lenses
    deleteConditionalForwarder_directoryId,
    deleteConditionalForwarder_remoteDomainName,

    -- * Destructuring the Response
    DeleteConditionalForwarderResponse (..),
    newDeleteConditionalForwarderResponse,

    -- * Response Lenses
    deleteConditionalForwarderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Deletes a conditional forwarder.
--
-- /See:/ 'newDeleteConditionalForwarder' smart constructor.
data DeleteConditionalForwarder = DeleteConditionalForwarder'
  { -- | The directory ID for which you are deleting the conditional forwarder.
    directoryId :: Prelude.Text,
    -- | The fully qualified domain name (FQDN) of the remote domain with which
    -- you are deleting the conditional forwarder.
    remoteDomainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConditionalForwarder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'deleteConditionalForwarder_directoryId' - The directory ID for which you are deleting the conditional forwarder.
--
-- 'remoteDomainName', 'deleteConditionalForwarder_remoteDomainName' - The fully qualified domain name (FQDN) of the remote domain with which
-- you are deleting the conditional forwarder.
newDeleteConditionalForwarder ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'remoteDomainName'
  Prelude.Text ->
  DeleteConditionalForwarder
newDeleteConditionalForwarder
  pDirectoryId_
  pRemoteDomainName_ =
    DeleteConditionalForwarder'
      { directoryId =
          pDirectoryId_,
        remoteDomainName = pRemoteDomainName_
      }

-- | The directory ID for which you are deleting the conditional forwarder.
deleteConditionalForwarder_directoryId :: Lens.Lens' DeleteConditionalForwarder Prelude.Text
deleteConditionalForwarder_directoryId = Lens.lens (\DeleteConditionalForwarder' {directoryId} -> directoryId) (\s@DeleteConditionalForwarder' {} a -> s {directoryId = a} :: DeleteConditionalForwarder)

-- | The fully qualified domain name (FQDN) of the remote domain with which
-- you are deleting the conditional forwarder.
deleteConditionalForwarder_remoteDomainName :: Lens.Lens' DeleteConditionalForwarder Prelude.Text
deleteConditionalForwarder_remoteDomainName = Lens.lens (\DeleteConditionalForwarder' {remoteDomainName} -> remoteDomainName) (\s@DeleteConditionalForwarder' {} a -> s {remoteDomainName = a} :: DeleteConditionalForwarder)

instance Core.AWSRequest DeleteConditionalForwarder where
  type
    AWSResponse DeleteConditionalForwarder =
      DeleteConditionalForwarderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConditionalForwarderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteConditionalForwarder where
  hashWithSalt _salt DeleteConditionalForwarder' {..} =
    _salt
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` remoteDomainName

instance Prelude.NFData DeleteConditionalForwarder where
  rnf DeleteConditionalForwarder' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf remoteDomainName

instance Data.ToHeaders DeleteConditionalForwarder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.DeleteConditionalForwarder" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteConditionalForwarder where
  toJSON DeleteConditionalForwarder' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just
              ("RemoteDomainName" Data..= remoteDomainName)
          ]
      )

instance Data.ToPath DeleteConditionalForwarder where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteConditionalForwarder where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a DeleteConditionalForwarder request.
--
-- /See:/ 'newDeleteConditionalForwarderResponse' smart constructor.
data DeleteConditionalForwarderResponse = DeleteConditionalForwarderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConditionalForwarderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteConditionalForwarderResponse_httpStatus' - The response's http status code.
newDeleteConditionalForwarderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteConditionalForwarderResponse
newDeleteConditionalForwarderResponse pHttpStatus_ =
  DeleteConditionalForwarderResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteConditionalForwarderResponse_httpStatus :: Lens.Lens' DeleteConditionalForwarderResponse Prelude.Int
deleteConditionalForwarderResponse_httpStatus = Lens.lens (\DeleteConditionalForwarderResponse' {httpStatus} -> httpStatus) (\s@DeleteConditionalForwarderResponse' {} a -> s {httpStatus = a} :: DeleteConditionalForwarderResponse)

instance
  Prelude.NFData
    DeleteConditionalForwarderResponse
  where
  rnf DeleteConditionalForwarderResponse' {..} =
    Prelude.rnf httpStatus
