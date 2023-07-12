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
-- Module      : Amazonka.Greengrass.DeleteResourceDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource definition.
module Amazonka.Greengrass.DeleteResourceDefinition
  ( -- * Creating a Request
    DeleteResourceDefinition (..),
    newDeleteResourceDefinition,

    -- * Request Lenses
    deleteResourceDefinition_resourceDefinitionId,

    -- * Destructuring the Response
    DeleteResourceDefinitionResponse (..),
    newDeleteResourceDefinitionResponse,

    -- * Response Lenses
    deleteResourceDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteResourceDefinition' smart constructor.
data DeleteResourceDefinition = DeleteResourceDefinition'
  { -- | The ID of the resource definition.
    resourceDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourceDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceDefinitionId', 'deleteResourceDefinition_resourceDefinitionId' - The ID of the resource definition.
newDeleteResourceDefinition ::
  -- | 'resourceDefinitionId'
  Prelude.Text ->
  DeleteResourceDefinition
newDeleteResourceDefinition pResourceDefinitionId_ =
  DeleteResourceDefinition'
    { resourceDefinitionId =
        pResourceDefinitionId_
    }

-- | The ID of the resource definition.
deleteResourceDefinition_resourceDefinitionId :: Lens.Lens' DeleteResourceDefinition Prelude.Text
deleteResourceDefinition_resourceDefinitionId = Lens.lens (\DeleteResourceDefinition' {resourceDefinitionId} -> resourceDefinitionId) (\s@DeleteResourceDefinition' {} a -> s {resourceDefinitionId = a} :: DeleteResourceDefinition)

instance Core.AWSRequest DeleteResourceDefinition where
  type
    AWSResponse DeleteResourceDefinition =
      DeleteResourceDefinitionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteResourceDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteResourceDefinition where
  hashWithSalt _salt DeleteResourceDefinition' {..} =
    _salt `Prelude.hashWithSalt` resourceDefinitionId

instance Prelude.NFData DeleteResourceDefinition where
  rnf DeleteResourceDefinition' {..} =
    Prelude.rnf resourceDefinitionId

instance Data.ToHeaders DeleteResourceDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteResourceDefinition where
  toPath DeleteResourceDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/resources/",
        Data.toBS resourceDefinitionId
      ]

instance Data.ToQuery DeleteResourceDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResourceDefinitionResponse' smart constructor.
data DeleteResourceDefinitionResponse = DeleteResourceDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourceDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteResourceDefinitionResponse_httpStatus' - The response's http status code.
newDeleteResourceDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteResourceDefinitionResponse
newDeleteResourceDefinitionResponse pHttpStatus_ =
  DeleteResourceDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteResourceDefinitionResponse_httpStatus :: Lens.Lens' DeleteResourceDefinitionResponse Prelude.Int
deleteResourceDefinitionResponse_httpStatus = Lens.lens (\DeleteResourceDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeleteResourceDefinitionResponse' {} a -> s {httpStatus = a} :: DeleteResourceDefinitionResponse)

instance
  Prelude.NFData
    DeleteResourceDefinitionResponse
  where
  rnf DeleteResourceDefinitionResponse' {..} =
    Prelude.rnf httpStatus
