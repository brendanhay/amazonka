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
-- Module      : Amazonka.SageMaker.DeleteModelBiasJobDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon SageMaker model bias job definition.
module Amazonka.SageMaker.DeleteModelBiasJobDefinition
  ( -- * Creating a Request
    DeleteModelBiasJobDefinition (..),
    newDeleteModelBiasJobDefinition,

    -- * Request Lenses
    deleteModelBiasJobDefinition_jobDefinitionName,

    -- * Destructuring the Response
    DeleteModelBiasJobDefinitionResponse (..),
    newDeleteModelBiasJobDefinitionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteModelBiasJobDefinition' smart constructor.
data DeleteModelBiasJobDefinition = DeleteModelBiasJobDefinition'
  { -- | The name of the model bias job definition to delete.
    jobDefinitionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelBiasJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobDefinitionName', 'deleteModelBiasJobDefinition_jobDefinitionName' - The name of the model bias job definition to delete.
newDeleteModelBiasJobDefinition ::
  -- | 'jobDefinitionName'
  Prelude.Text ->
  DeleteModelBiasJobDefinition
newDeleteModelBiasJobDefinition pJobDefinitionName_ =
  DeleteModelBiasJobDefinition'
    { jobDefinitionName =
        pJobDefinitionName_
    }

-- | The name of the model bias job definition to delete.
deleteModelBiasJobDefinition_jobDefinitionName :: Lens.Lens' DeleteModelBiasJobDefinition Prelude.Text
deleteModelBiasJobDefinition_jobDefinitionName = Lens.lens (\DeleteModelBiasJobDefinition' {jobDefinitionName} -> jobDefinitionName) (\s@DeleteModelBiasJobDefinition' {} a -> s {jobDefinitionName = a} :: DeleteModelBiasJobDefinition)

instance Core.AWSRequest DeleteModelBiasJobDefinition where
  type
    AWSResponse DeleteModelBiasJobDefinition =
      DeleteModelBiasJobDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteModelBiasJobDefinitionResponse'

instance
  Prelude.Hashable
    DeleteModelBiasJobDefinition
  where
  hashWithSalt _salt DeleteModelBiasJobDefinition' {..} =
    _salt `Prelude.hashWithSalt` jobDefinitionName

instance Prelude.NFData DeleteModelBiasJobDefinition where
  rnf DeleteModelBiasJobDefinition' {..} =
    Prelude.rnf jobDefinitionName

instance Core.ToHeaders DeleteModelBiasJobDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DeleteModelBiasJobDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteModelBiasJobDefinition where
  toJSON DeleteModelBiasJobDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("JobDefinitionName" Core..= jobDefinitionName)
          ]
      )

instance Core.ToPath DeleteModelBiasJobDefinition where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteModelBiasJobDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteModelBiasJobDefinitionResponse' smart constructor.
data DeleteModelBiasJobDefinitionResponse = DeleteModelBiasJobDefinitionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelBiasJobDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteModelBiasJobDefinitionResponse ::
  DeleteModelBiasJobDefinitionResponse
newDeleteModelBiasJobDefinitionResponse =
  DeleteModelBiasJobDefinitionResponse'

instance
  Prelude.NFData
    DeleteModelBiasJobDefinitionResponse
  where
  rnf _ = ()
