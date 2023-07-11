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
-- Module      : Amazonka.Greengrass.DeleteCoreDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a core definition.
module Amazonka.Greengrass.DeleteCoreDefinition
  ( -- * Creating a Request
    DeleteCoreDefinition (..),
    newDeleteCoreDefinition,

    -- * Request Lenses
    deleteCoreDefinition_coreDefinitionId,

    -- * Destructuring the Response
    DeleteCoreDefinitionResponse (..),
    newDeleteCoreDefinitionResponse,

    -- * Response Lenses
    deleteCoreDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCoreDefinition' smart constructor.
data DeleteCoreDefinition = DeleteCoreDefinition'
  { -- | The ID of the core definition.
    coreDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCoreDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreDefinitionId', 'deleteCoreDefinition_coreDefinitionId' - The ID of the core definition.
newDeleteCoreDefinition ::
  -- | 'coreDefinitionId'
  Prelude.Text ->
  DeleteCoreDefinition
newDeleteCoreDefinition pCoreDefinitionId_ =
  DeleteCoreDefinition'
    { coreDefinitionId =
        pCoreDefinitionId_
    }

-- | The ID of the core definition.
deleteCoreDefinition_coreDefinitionId :: Lens.Lens' DeleteCoreDefinition Prelude.Text
deleteCoreDefinition_coreDefinitionId = Lens.lens (\DeleteCoreDefinition' {coreDefinitionId} -> coreDefinitionId) (\s@DeleteCoreDefinition' {} a -> s {coreDefinitionId = a} :: DeleteCoreDefinition)

instance Core.AWSRequest DeleteCoreDefinition where
  type
    AWSResponse DeleteCoreDefinition =
      DeleteCoreDefinitionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCoreDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCoreDefinition where
  hashWithSalt _salt DeleteCoreDefinition' {..} =
    _salt `Prelude.hashWithSalt` coreDefinitionId

instance Prelude.NFData DeleteCoreDefinition where
  rnf DeleteCoreDefinition' {..} =
    Prelude.rnf coreDefinitionId

instance Data.ToHeaders DeleteCoreDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteCoreDefinition where
  toPath DeleteCoreDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/cores/",
        Data.toBS coreDefinitionId
      ]

instance Data.ToQuery DeleteCoreDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCoreDefinitionResponse' smart constructor.
data DeleteCoreDefinitionResponse = DeleteCoreDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCoreDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCoreDefinitionResponse_httpStatus' - The response's http status code.
newDeleteCoreDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCoreDefinitionResponse
newDeleteCoreDefinitionResponse pHttpStatus_ =
  DeleteCoreDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteCoreDefinitionResponse_httpStatus :: Lens.Lens' DeleteCoreDefinitionResponse Prelude.Int
deleteCoreDefinitionResponse_httpStatus = Lens.lens (\DeleteCoreDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeleteCoreDefinitionResponse' {} a -> s {httpStatus = a} :: DeleteCoreDefinitionResponse)

instance Prelude.NFData DeleteCoreDefinitionResponse where
  rnf DeleteCoreDefinitionResponse' {..} =
    Prelude.rnf httpStatus
