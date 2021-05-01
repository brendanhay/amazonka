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
-- Module      : Network.AWS.SageMaker.DeleteModelExplainabilityJobDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon SageMaker model explainability job definition.
module Network.AWS.SageMaker.DeleteModelExplainabilityJobDefinition
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteModelExplainabilityJobDefinition' smart constructor.
data DeleteModelExplainabilityJobDefinition = DeleteModelExplainabilityJobDefinition'
  { -- | The name of the model explainability job definition to delete.
    jobDefinitionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    DeleteModelExplainabilityJobDefinition
  where
  type
    Rs DeleteModelExplainabilityJobDefinition =
      DeleteModelExplainabilityJobDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteModelExplainabilityJobDefinitionResponse'

instance
  Prelude.Hashable
    DeleteModelExplainabilityJobDefinition

instance
  Prelude.NFData
    DeleteModelExplainabilityJobDefinition

instance
  Prelude.ToHeaders
    DeleteModelExplainabilityJobDefinition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DeleteModelExplainabilityJobDefinition" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DeleteModelExplainabilityJobDefinition
  where
  toJSON DeleteModelExplainabilityJobDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("JobDefinitionName" Prelude..= jobDefinitionName)
          ]
      )

instance
  Prelude.ToPath
    DeleteModelExplainabilityJobDefinition
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteModelExplainabilityJobDefinition
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteModelExplainabilityJobDefinitionResponse' smart constructor.
data DeleteModelExplainabilityJobDefinitionResponse = DeleteModelExplainabilityJobDefinitionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
