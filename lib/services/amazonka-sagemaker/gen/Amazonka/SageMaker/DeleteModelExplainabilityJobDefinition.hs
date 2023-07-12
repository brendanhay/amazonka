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
-- Module      : Amazonka.SageMaker.DeleteModelExplainabilityJobDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon SageMaker model explainability job definition.
module Amazonka.SageMaker.DeleteModelExplainabilityJobDefinition
  ( -- * Creating a Request
    DeleteModelExplainabilityJobDefinition (..),
    newDeleteModelExplainabilityJobDefinition,

    -- * Request Lenses
    deleteModelExplainabilityJobDefinition_jobDefinitionName,

    -- * Destructuring the Response
    DeleteModelExplainabilityJobDefinitionResponse (..),
    newDeleteModelExplainabilityJobDefinitionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteModelExplainabilityJobDefinition' smart constructor.
data DeleteModelExplainabilityJobDefinition = DeleteModelExplainabilityJobDefinition'
  { -- | The name of the model explainability job definition to delete.
    jobDefinitionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelExplainabilityJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobDefinitionName', 'deleteModelExplainabilityJobDefinition_jobDefinitionName' - The name of the model explainability job definition to delete.
newDeleteModelExplainabilityJobDefinition ::
  -- | 'jobDefinitionName'
  Prelude.Text ->
  DeleteModelExplainabilityJobDefinition
newDeleteModelExplainabilityJobDefinition
  pJobDefinitionName_ =
    DeleteModelExplainabilityJobDefinition'
      { jobDefinitionName =
          pJobDefinitionName_
      }

-- | The name of the model explainability job definition to delete.
deleteModelExplainabilityJobDefinition_jobDefinitionName :: Lens.Lens' DeleteModelExplainabilityJobDefinition Prelude.Text
deleteModelExplainabilityJobDefinition_jobDefinitionName = Lens.lens (\DeleteModelExplainabilityJobDefinition' {jobDefinitionName} -> jobDefinitionName) (\s@DeleteModelExplainabilityJobDefinition' {} a -> s {jobDefinitionName = a} :: DeleteModelExplainabilityJobDefinition)

instance
  Core.AWSRequest
    DeleteModelExplainabilityJobDefinition
  where
  type
    AWSResponse
      DeleteModelExplainabilityJobDefinition =
      DeleteModelExplainabilityJobDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteModelExplainabilityJobDefinitionResponse'

instance
  Prelude.Hashable
    DeleteModelExplainabilityJobDefinition
  where
  hashWithSalt
    _salt
    DeleteModelExplainabilityJobDefinition' {..} =
      _salt `Prelude.hashWithSalt` jobDefinitionName

instance
  Prelude.NFData
    DeleteModelExplainabilityJobDefinition
  where
  rnf DeleteModelExplainabilityJobDefinition' {..} =
    Prelude.rnf jobDefinitionName

instance
  Data.ToHeaders
    DeleteModelExplainabilityJobDefinition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DeleteModelExplainabilityJobDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DeleteModelExplainabilityJobDefinition
  where
  toJSON DeleteModelExplainabilityJobDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("JobDefinitionName" Data..= jobDefinitionName)
          ]
      )

instance
  Data.ToPath
    DeleteModelExplainabilityJobDefinition
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteModelExplainabilityJobDefinition
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteModelExplainabilityJobDefinitionResponse' smart constructor.
data DeleteModelExplainabilityJobDefinitionResponse = DeleteModelExplainabilityJobDefinitionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelExplainabilityJobDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteModelExplainabilityJobDefinitionResponse ::
  DeleteModelExplainabilityJobDefinitionResponse
newDeleteModelExplainabilityJobDefinitionResponse =
  DeleteModelExplainabilityJobDefinitionResponse'

instance
  Prelude.NFData
    DeleteModelExplainabilityJobDefinitionResponse
  where
  rnf _ = ()
