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
-- Module      : Amazonka.DirectoryService.EnableLDAPS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates the switch for the specific directory to always use LDAP
-- secure calls.
module Amazonka.DirectoryService.EnableLDAPS
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableLDAPS' smart constructor.
data EnableLDAPS = EnableLDAPS'
  { -- | The identifier of the directory.
    directoryId :: Prelude.Text,
    -- | The type of LDAP security to enable. Currently only the value @Client@
    -- is supported.
    type' :: LDAPSType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'type''
  LDAPSType ->
  EnableLDAPS
newEnableLDAPS pDirectoryId_ pType_ =
  EnableLDAPS'
    { directoryId = pDirectoryId_,
      type' = pType_
    }

-- | The identifier of the directory.
enableLDAPS_directoryId :: Lens.Lens' EnableLDAPS Prelude.Text
enableLDAPS_directoryId = Lens.lens (\EnableLDAPS' {directoryId} -> directoryId) (\s@EnableLDAPS' {} a -> s {directoryId = a} :: EnableLDAPS)

-- | The type of LDAP security to enable. Currently only the value @Client@
-- is supported.
enableLDAPS_type :: Lens.Lens' EnableLDAPS LDAPSType
enableLDAPS_type = Lens.lens (\EnableLDAPS' {type'} -> type') (\s@EnableLDAPS' {} a -> s {type' = a} :: EnableLDAPS)

instance Core.AWSRequest EnableLDAPS where
  type AWSResponse EnableLDAPS = EnableLDAPSResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableLDAPSResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableLDAPS where
  hashWithSalt _salt EnableLDAPS' {..} =
    _salt
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData EnableLDAPS where
  rnf EnableLDAPS' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders EnableLDAPS where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.EnableLDAPS" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EnableLDAPS where
  toJSON EnableLDAPS' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just ("Type" Data..= type')
          ]
      )

instance Data.ToPath EnableLDAPS where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableLDAPS where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableLDAPSResponse' smart constructor.
data EnableLDAPSResponse = EnableLDAPSResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  EnableLDAPSResponse
newEnableLDAPSResponse pHttpStatus_ =
  EnableLDAPSResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
enableLDAPSResponse_httpStatus :: Lens.Lens' EnableLDAPSResponse Prelude.Int
enableLDAPSResponse_httpStatus = Lens.lens (\EnableLDAPSResponse' {httpStatus} -> httpStatus) (\s@EnableLDAPSResponse' {} a -> s {httpStatus = a} :: EnableLDAPSResponse)

instance Prelude.NFData EnableLDAPSResponse where
  rnf EnableLDAPSResponse' {..} = Prelude.rnf httpStatus
