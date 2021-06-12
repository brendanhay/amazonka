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
-- Module      : Network.AWS.DirectoryService.EnableLDAPS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates the switch for the specific directory to always use LDAP
-- secure calls.
module Network.AWS.DirectoryService.EnableLDAPS
  ( -- * Creating a Request
    EnableLDAPS (..),
    newEnableLDAPS,

    -- * Request Lenses
    enableLDAPS_directoryId,
    enableLDAPS_type,

    -- * Destructuring the Response
    EnableLDAPSResponse (..),
    newEnableLDAPSResponse,

    -- * Response Lenses
    enableLDAPSResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableLDAPS' smart constructor.
data EnableLDAPS = EnableLDAPS'
  { -- | The identifier of the directory.
    directoryId :: Core.Text,
    -- | The type of LDAP security to enable. Currently only the value @Client@
    -- is supported.
    type' :: LDAPSType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableLDAPS' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'enableLDAPS_directoryId' - The identifier of the directory.
--
-- 'type'', 'enableLDAPS_type' - The type of LDAP security to enable. Currently only the value @Client@
-- is supported.
newEnableLDAPS ::
  -- | 'directoryId'
  Core.Text ->
  -- | 'type''
  LDAPSType ->
  EnableLDAPS
newEnableLDAPS pDirectoryId_ pType_ =
  EnableLDAPS'
    { directoryId = pDirectoryId_,
      type' = pType_
    }

-- | The identifier of the directory.
enableLDAPS_directoryId :: Lens.Lens' EnableLDAPS Core.Text
enableLDAPS_directoryId = Lens.lens (\EnableLDAPS' {directoryId} -> directoryId) (\s@EnableLDAPS' {} a -> s {directoryId = a} :: EnableLDAPS)

-- | The type of LDAP security to enable. Currently only the value @Client@
-- is supported.
enableLDAPS_type :: Lens.Lens' EnableLDAPS LDAPSType
enableLDAPS_type = Lens.lens (\EnableLDAPS' {type'} -> type') (\s@EnableLDAPS' {} a -> s {type' = a} :: EnableLDAPS)

instance Core.AWSRequest EnableLDAPS where
  type AWSResponse EnableLDAPS = EnableLDAPSResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableLDAPSResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable EnableLDAPS

instance Core.NFData EnableLDAPS

instance Core.ToHeaders EnableLDAPS where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.EnableLDAPS" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON EnableLDAPS where
  toJSON EnableLDAPS' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("Type" Core..= type')
          ]
      )

instance Core.ToPath EnableLDAPS where
  toPath = Core.const "/"

instance Core.ToQuery EnableLDAPS where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newEnableLDAPSResponse' smart constructor.
data EnableLDAPSResponse = EnableLDAPSResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableLDAPSResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableLDAPSResponse_httpStatus' - The response's http status code.
newEnableLDAPSResponse ::
  -- | 'httpStatus'
  Core.Int ->
  EnableLDAPSResponse
newEnableLDAPSResponse pHttpStatus_ =
  EnableLDAPSResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
enableLDAPSResponse_httpStatus :: Lens.Lens' EnableLDAPSResponse Core.Int
enableLDAPSResponse_httpStatus = Lens.lens (\EnableLDAPSResponse' {httpStatus} -> httpStatus) (\s@EnableLDAPSResponse' {} a -> s {httpStatus = a} :: EnableLDAPSResponse)

instance Core.NFData EnableLDAPSResponse
