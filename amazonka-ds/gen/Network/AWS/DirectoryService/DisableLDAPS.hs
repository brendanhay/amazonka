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
-- Module      : Network.AWS.DirectoryService.DisableLDAPS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates LDAP secure calls for the specified directory.
module Network.AWS.DirectoryService.DisableLDAPS
  ( -- * Creating a Request
    DisableLDAPS (..),
    newDisableLDAPS,

    -- * Request Lenses
    disableLDAPS_directoryId,
    disableLDAPS_type,

    -- * Destructuring the Response
    DisableLDAPSResponse (..),
    newDisableLDAPSResponse,

    -- * Response Lenses
    disableLDAPSResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisableLDAPS' smart constructor.
data DisableLDAPS = DisableLDAPS'
  { -- | The identifier of the directory.
    directoryId :: Core.Text,
    -- | The type of LDAP security to enable. Currently only the value @Client@
    -- is supported.
    type' :: LDAPSType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableLDAPS' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'disableLDAPS_directoryId' - The identifier of the directory.
--
-- 'type'', 'disableLDAPS_type' - The type of LDAP security to enable. Currently only the value @Client@
-- is supported.
newDisableLDAPS ::
  -- | 'directoryId'
  Core.Text ->
  -- | 'type''
  LDAPSType ->
  DisableLDAPS
newDisableLDAPS pDirectoryId_ pType_ =
  DisableLDAPS'
    { directoryId = pDirectoryId_,
      type' = pType_
    }

-- | The identifier of the directory.
disableLDAPS_directoryId :: Lens.Lens' DisableLDAPS Core.Text
disableLDAPS_directoryId = Lens.lens (\DisableLDAPS' {directoryId} -> directoryId) (\s@DisableLDAPS' {} a -> s {directoryId = a} :: DisableLDAPS)

-- | The type of LDAP security to enable. Currently only the value @Client@
-- is supported.
disableLDAPS_type :: Lens.Lens' DisableLDAPS LDAPSType
disableLDAPS_type = Lens.lens (\DisableLDAPS' {type'} -> type') (\s@DisableLDAPS' {} a -> s {type' = a} :: DisableLDAPS)

instance Core.AWSRequest DisableLDAPS where
  type AWSResponse DisableLDAPS = DisableLDAPSResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableLDAPSResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisableLDAPS

instance Core.NFData DisableLDAPS

instance Core.ToHeaders DisableLDAPS where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DisableLDAPS" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisableLDAPS where
  toJSON DisableLDAPS' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("Type" Core..= type')
          ]
      )

instance Core.ToPath DisableLDAPS where
  toPath = Core.const "/"

instance Core.ToQuery DisableLDAPS where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisableLDAPSResponse' smart constructor.
data DisableLDAPSResponse = DisableLDAPSResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableLDAPSResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disableLDAPSResponse_httpStatus' - The response's http status code.
newDisableLDAPSResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisableLDAPSResponse
newDisableLDAPSResponse pHttpStatus_ =
  DisableLDAPSResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
disableLDAPSResponse_httpStatus :: Lens.Lens' DisableLDAPSResponse Core.Int
disableLDAPSResponse_httpStatus = Lens.lens (\DisableLDAPSResponse' {httpStatus} -> httpStatus) (\s@DisableLDAPSResponse' {} a -> s {httpStatus = a} :: DisableLDAPSResponse)

instance Core.NFData DisableLDAPSResponse
