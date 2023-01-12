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
-- Module      : Amazonka.Kendra.DeleteAccessControlConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an access control configuration that you created for your
-- documents in an index. This includes user and group access information
-- for your documents. This is useful for user context filtering, where
-- search results are filtered based on the user or their group access to
-- documents.
module Amazonka.Kendra.DeleteAccessControlConfiguration
  ( -- * Creating a Request
    DeleteAccessControlConfiguration (..),
    newDeleteAccessControlConfiguration,

    -- * Request Lenses
    deleteAccessControlConfiguration_indexId,
    deleteAccessControlConfiguration_id,

    -- * Destructuring the Response
    DeleteAccessControlConfigurationResponse (..),
    newDeleteAccessControlConfigurationResponse,

    -- * Response Lenses
    deleteAccessControlConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAccessControlConfiguration' smart constructor.
data DeleteAccessControlConfiguration = DeleteAccessControlConfiguration'
  { -- | The identifier of the index for an access control configuration.
    indexId :: Prelude.Text,
    -- | The identifier of the access control configuration you want to delete.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessControlConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexId', 'deleteAccessControlConfiguration_indexId' - The identifier of the index for an access control configuration.
--
-- 'id', 'deleteAccessControlConfiguration_id' - The identifier of the access control configuration you want to delete.
newDeleteAccessControlConfiguration ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  DeleteAccessControlConfiguration
newDeleteAccessControlConfiguration pIndexId_ pId_ =
  DeleteAccessControlConfiguration'
    { indexId =
        pIndexId_,
      id = pId_
    }

-- | The identifier of the index for an access control configuration.
deleteAccessControlConfiguration_indexId :: Lens.Lens' DeleteAccessControlConfiguration Prelude.Text
deleteAccessControlConfiguration_indexId = Lens.lens (\DeleteAccessControlConfiguration' {indexId} -> indexId) (\s@DeleteAccessControlConfiguration' {} a -> s {indexId = a} :: DeleteAccessControlConfiguration)

-- | The identifier of the access control configuration you want to delete.
deleteAccessControlConfiguration_id :: Lens.Lens' DeleteAccessControlConfiguration Prelude.Text
deleteAccessControlConfiguration_id = Lens.lens (\DeleteAccessControlConfiguration' {id} -> id) (\s@DeleteAccessControlConfiguration' {} a -> s {id = a} :: DeleteAccessControlConfiguration)

instance
  Core.AWSRequest
    DeleteAccessControlConfiguration
  where
  type
    AWSResponse DeleteAccessControlConfiguration =
      DeleteAccessControlConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAccessControlConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteAccessControlConfiguration
  where
  hashWithSalt
    _salt
    DeleteAccessControlConfiguration' {..} =
      _salt `Prelude.hashWithSalt` indexId
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    DeleteAccessControlConfiguration
  where
  rnf DeleteAccessControlConfiguration' {..} =
    Prelude.rnf indexId `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    DeleteAccessControlConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.DeleteAccessControlConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAccessControlConfiguration where
  toJSON DeleteAccessControlConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath DeleteAccessControlConfiguration where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteAccessControlConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAccessControlConfigurationResponse' smart constructor.
data DeleteAccessControlConfigurationResponse = DeleteAccessControlConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessControlConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAccessControlConfigurationResponse_httpStatus' - The response's http status code.
newDeleteAccessControlConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAccessControlConfigurationResponse
newDeleteAccessControlConfigurationResponse
  pHttpStatus_ =
    DeleteAccessControlConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteAccessControlConfigurationResponse_httpStatus :: Lens.Lens' DeleteAccessControlConfigurationResponse Prelude.Int
deleteAccessControlConfigurationResponse_httpStatus = Lens.lens (\DeleteAccessControlConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteAccessControlConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteAccessControlConfigurationResponse)

instance
  Prelude.NFData
    DeleteAccessControlConfigurationResponse
  where
  rnf DeleteAccessControlConfigurationResponse' {..} =
    Prelude.rnf httpStatus
