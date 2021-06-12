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
-- Module      : Network.AWS.Shield.DeleteProtection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Shield Advanced Protection.
module Network.AWS.Shield.DeleteProtection
  ( -- * Creating a Request
    DeleteProtection (..),
    newDeleteProtection,

    -- * Request Lenses
    deleteProtection_protectionId,

    -- * Destructuring the Response
    DeleteProtectionResponse (..),
    newDeleteProtectionResponse,

    -- * Response Lenses
    deleteProtectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newDeleteProtection' smart constructor.
data DeleteProtection = DeleteProtection'
  { -- | The unique identifier (ID) for the Protection object to be deleted.
    protectionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProtection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protectionId', 'deleteProtection_protectionId' - The unique identifier (ID) for the Protection object to be deleted.
newDeleteProtection ::
  -- | 'protectionId'
  Core.Text ->
  DeleteProtection
newDeleteProtection pProtectionId_ =
  DeleteProtection' {protectionId = pProtectionId_}

-- | The unique identifier (ID) for the Protection object to be deleted.
deleteProtection_protectionId :: Lens.Lens' DeleteProtection Core.Text
deleteProtection_protectionId = Lens.lens (\DeleteProtection' {protectionId} -> protectionId) (\s@DeleteProtection' {} a -> s {protectionId = a} :: DeleteProtection)

instance Core.AWSRequest DeleteProtection where
  type
    AWSResponse DeleteProtection =
      DeleteProtectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProtectionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteProtection

instance Core.NFData DeleteProtection

instance Core.ToHeaders DeleteProtection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.DeleteProtection" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteProtection where
  toJSON DeleteProtection' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ProtectionId" Core..= protectionId)]
      )

instance Core.ToPath DeleteProtection where
  toPath = Core.const "/"

instance Core.ToQuery DeleteProtection where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteProtectionResponse' smart constructor.
data DeleteProtectionResponse = DeleteProtectionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProtectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteProtectionResponse_httpStatus' - The response's http status code.
newDeleteProtectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteProtectionResponse
newDeleteProtectionResponse pHttpStatus_ =
  DeleteProtectionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteProtectionResponse_httpStatus :: Lens.Lens' DeleteProtectionResponse Core.Int
deleteProtectionResponse_httpStatus = Lens.lens (\DeleteProtectionResponse' {httpStatus} -> httpStatus) (\s@DeleteProtectionResponse' {} a -> s {httpStatus = a} :: DeleteProtectionResponse)

instance Core.NFData DeleteProtectionResponse
