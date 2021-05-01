{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Greengrass.DeleteCoreDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a core definition.
module Network.AWS.Greengrass.DeleteCoreDefinition
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

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCoreDefinition' smart constructor.
data DeleteCoreDefinition = DeleteCoreDefinition'
  { -- | The ID of the core definition.
    coreDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteCoreDefinition where
  type
    Rs DeleteCoreDefinition =
      DeleteCoreDefinitionResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCoreDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCoreDefinition

instance Prelude.NFData DeleteCoreDefinition

instance Prelude.ToHeaders DeleteCoreDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteCoreDefinition where
  toPath DeleteCoreDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/cores/",
        Prelude.toBS coreDefinitionId
      ]

instance Prelude.ToQuery DeleteCoreDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCoreDefinitionResponse' smart constructor.
data DeleteCoreDefinitionResponse = DeleteCoreDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteCoreDefinitionResponse
