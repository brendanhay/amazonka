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
-- Module      : Network.AWS.DirectoryService.DeleteConditionalForwarder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a conditional forwarder that has been set up for your AWS
-- directory.
module Network.AWS.DirectoryService.DeleteConditionalForwarder
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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes a conditional forwarder.
--
-- /See:/ 'newDeleteConditionalForwarder' smart constructor.
data DeleteConditionalForwarder = DeleteConditionalForwarder'
  { -- | The directory ID for which you are deleting the conditional forwarder.
    directoryId :: Core.Text,
    -- | The fully qualified domain name (FQDN) of the remote domain with which
    -- you are deleting the conditional forwarder.
    remoteDomainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'remoteDomainName'
  Core.Text ->
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
deleteConditionalForwarder_directoryId :: Lens.Lens' DeleteConditionalForwarder Core.Text
deleteConditionalForwarder_directoryId = Lens.lens (\DeleteConditionalForwarder' {directoryId} -> directoryId) (\s@DeleteConditionalForwarder' {} a -> s {directoryId = a} :: DeleteConditionalForwarder)

-- | The fully qualified domain name (FQDN) of the remote domain with which
-- you are deleting the conditional forwarder.
deleteConditionalForwarder_remoteDomainName :: Lens.Lens' DeleteConditionalForwarder Core.Text
deleteConditionalForwarder_remoteDomainName = Lens.lens (\DeleteConditionalForwarder' {remoteDomainName} -> remoteDomainName) (\s@DeleteConditionalForwarder' {} a -> s {remoteDomainName = a} :: DeleteConditionalForwarder)

instance Core.AWSRequest DeleteConditionalForwarder where
  type
    AWSResponse DeleteConditionalForwarder =
      DeleteConditionalForwarderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConditionalForwarderResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteConditionalForwarder

instance Core.NFData DeleteConditionalForwarder

instance Core.ToHeaders DeleteConditionalForwarder where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DeleteConditionalForwarder" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteConditionalForwarder where
  toJSON DeleteConditionalForwarder' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just
              ("RemoteDomainName" Core..= remoteDomainName)
          ]
      )

instance Core.ToPath DeleteConditionalForwarder where
  toPath = Core.const "/"

instance Core.ToQuery DeleteConditionalForwarder where
  toQuery = Core.const Core.mempty

-- | The result of a DeleteConditionalForwarder request.
--
-- /See:/ 'newDeleteConditionalForwarderResponse' smart constructor.
data DeleteConditionalForwarderResponse = DeleteConditionalForwarderResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteConditionalForwarderResponse
newDeleteConditionalForwarderResponse pHttpStatus_ =
  DeleteConditionalForwarderResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteConditionalForwarderResponse_httpStatus :: Lens.Lens' DeleteConditionalForwarderResponse Core.Int
deleteConditionalForwarderResponse_httpStatus = Lens.lens (\DeleteConditionalForwarderResponse' {httpStatus} -> httpStatus) (\s@DeleteConditionalForwarderResponse' {} a -> s {httpStatus = a} :: DeleteConditionalForwarderResponse)

instance
  Core.NFData
    DeleteConditionalForwarderResponse
