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
-- Module      : Network.AWS.Lightsail.DeleteKnownHostKeys
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the known host key or certificate used by the Amazon Lightsail
-- browser-based SSH or RDP clients to authenticate an instance. This
-- operation enables the Lightsail browser-based SSH or RDP clients to
-- connect to the instance after a host key mismatch.
--
-- Perform this operation only if you were expecting the host key or
-- certificate mismatch or if you are familiar with the new host key or
-- certificate on the instance. For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-troubleshooting-browser-based-ssh-rdp-client-connection Troubleshooting connection issues when using the Amazon Lightsail browser-based SSH or RDP client>.
module Network.AWS.Lightsail.DeleteKnownHostKeys
  ( -- * Creating a Request
    DeleteKnownHostKeys (..),
    newDeleteKnownHostKeys,

    -- * Request Lenses
    deleteKnownHostKeys_instanceName,

    -- * Destructuring the Response
    DeleteKnownHostKeysResponse (..),
    newDeleteKnownHostKeysResponse,

    -- * Response Lenses
    deleteKnownHostKeysResponse_operations,
    deleteKnownHostKeysResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteKnownHostKeys' smart constructor.
data DeleteKnownHostKeys = DeleteKnownHostKeys'
  { -- | The name of the instance for which you want to reset the host key or
    -- certificate.
    instanceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteKnownHostKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceName', 'deleteKnownHostKeys_instanceName' - The name of the instance for which you want to reset the host key or
-- certificate.
newDeleteKnownHostKeys ::
  -- | 'instanceName'
  Core.Text ->
  DeleteKnownHostKeys
newDeleteKnownHostKeys pInstanceName_ =
  DeleteKnownHostKeys' {instanceName = pInstanceName_}

-- | The name of the instance for which you want to reset the host key or
-- certificate.
deleteKnownHostKeys_instanceName :: Lens.Lens' DeleteKnownHostKeys Core.Text
deleteKnownHostKeys_instanceName = Lens.lens (\DeleteKnownHostKeys' {instanceName} -> instanceName) (\s@DeleteKnownHostKeys' {} a -> s {instanceName = a} :: DeleteKnownHostKeys)

instance Core.AWSRequest DeleteKnownHostKeys where
  type
    AWSResponse DeleteKnownHostKeys =
      DeleteKnownHostKeysResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteKnownHostKeysResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteKnownHostKeys

instance Core.NFData DeleteKnownHostKeys

instance Core.ToHeaders DeleteKnownHostKeys where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DeleteKnownHostKeys" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteKnownHostKeys where
  toJSON DeleteKnownHostKeys' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("instanceName" Core..= instanceName)]
      )

instance Core.ToPath DeleteKnownHostKeys where
  toPath = Core.const "/"

instance Core.ToQuery DeleteKnownHostKeys where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteKnownHostKeysResponse' smart constructor.
data DeleteKnownHostKeysResponse = DeleteKnownHostKeysResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteKnownHostKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'deleteKnownHostKeysResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'deleteKnownHostKeysResponse_httpStatus' - The response's http status code.
newDeleteKnownHostKeysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteKnownHostKeysResponse
newDeleteKnownHostKeysResponse pHttpStatus_ =
  DeleteKnownHostKeysResponse'
    { operations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteKnownHostKeysResponse_operations :: Lens.Lens' DeleteKnownHostKeysResponse (Core.Maybe [Operation])
deleteKnownHostKeysResponse_operations = Lens.lens (\DeleteKnownHostKeysResponse' {operations} -> operations) (\s@DeleteKnownHostKeysResponse' {} a -> s {operations = a} :: DeleteKnownHostKeysResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteKnownHostKeysResponse_httpStatus :: Lens.Lens' DeleteKnownHostKeysResponse Core.Int
deleteKnownHostKeysResponse_httpStatus = Lens.lens (\DeleteKnownHostKeysResponse' {httpStatus} -> httpStatus) (\s@DeleteKnownHostKeysResponse' {} a -> s {httpStatus = a} :: DeleteKnownHostKeysResponse)

instance Core.NFData DeleteKnownHostKeysResponse
