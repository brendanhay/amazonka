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
-- Module      : Amazonka.DirectoryService.DisableLDAPS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates LDAP secure calls for the specified directory.
module Amazonka.DirectoryService.DisableLDAPS
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableLDAPS' smart constructor.
data DisableLDAPS = DisableLDAPS'
  { -- | The identifier of the directory.
    directoryId :: Prelude.Text,
    -- | The type of LDAP security to enable. Currently only the value @Client@
    -- is supported.
    type' :: LDAPSType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'type''
  LDAPSType ->
  DisableLDAPS
newDisableLDAPS pDirectoryId_ pType_ =
  DisableLDAPS'
    { directoryId = pDirectoryId_,
      type' = pType_
    }

-- | The identifier of the directory.
disableLDAPS_directoryId :: Lens.Lens' DisableLDAPS Prelude.Text
disableLDAPS_directoryId = Lens.lens (\DisableLDAPS' {directoryId} -> directoryId) (\s@DisableLDAPS' {} a -> s {directoryId = a} :: DisableLDAPS)

-- | The type of LDAP security to enable. Currently only the value @Client@
-- is supported.
disableLDAPS_type :: Lens.Lens' DisableLDAPS LDAPSType
disableLDAPS_type = Lens.lens (\DisableLDAPS' {type'} -> type') (\s@DisableLDAPS' {} a -> s {type' = a} :: DisableLDAPS)

instance Core.AWSRequest DisableLDAPS where
  type AWSResponse DisableLDAPS = DisableLDAPSResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableLDAPSResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableLDAPS where
  hashWithSalt _salt DisableLDAPS' {..} =
    _salt
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DisableLDAPS where
  rnf DisableLDAPS' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders DisableLDAPS where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.DisableLDAPS" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisableLDAPS where
  toJSON DisableLDAPS' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just ("Type" Data..= type')
          ]
      )

instance Data.ToPath DisableLDAPS where
  toPath = Prelude.const "/"

instance Data.ToQuery DisableLDAPS where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableLDAPSResponse' smart constructor.
data DisableLDAPSResponse = DisableLDAPSResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DisableLDAPSResponse
newDisableLDAPSResponse pHttpStatus_ =
  DisableLDAPSResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
disableLDAPSResponse_httpStatus :: Lens.Lens' DisableLDAPSResponse Prelude.Int
disableLDAPSResponse_httpStatus = Lens.lens (\DisableLDAPSResponse' {httpStatus} -> httpStatus) (\s@DisableLDAPSResponse' {} a -> s {httpStatus = a} :: DisableLDAPSResponse)

instance Prelude.NFData DisableLDAPSResponse where
  rnf DisableLDAPSResponse' {..} =
    Prelude.rnf httpStatus
