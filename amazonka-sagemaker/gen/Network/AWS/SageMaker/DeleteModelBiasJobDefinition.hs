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
-- Module      : Network.AWS.SageMaker.DeleteModelBiasJobDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon SageMaker model bias job definition.
module Network.AWS.SageMaker.DeleteModelBiasJobDefinition
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteModelBiasJobDefinition' smart constructor.
data DeleteModelBiasJobDefinition = DeleteModelBiasJobDefinition'
  { -- | The name of the model bias job definition to delete.
    jobDefinitionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.AWSRequest
    DeleteModelBiasJobDefinition
  where
  type
    Rs DeleteModelBiasJobDefinition =
      DeleteModelBiasJobDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteModelBiasJobDefinitionResponse'

instance
  Prelude.Hashable
    DeleteModelBiasJobDefinition

instance Prelude.NFData DeleteModelBiasJobDefinition

instance
  Prelude.ToHeaders
    DeleteModelBiasJobDefinition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DeleteModelBiasJobDefinition" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteModelBiasJobDefinition where
  toJSON DeleteModelBiasJobDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("JobDefinitionName" Prelude..= jobDefinitionName)
          ]
      )

instance Prelude.ToPath DeleteModelBiasJobDefinition where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteModelBiasJobDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteModelBiasJobDefinitionResponse' smart constructor.
data DeleteModelBiasJobDefinitionResponse = DeleteModelBiasJobDefinitionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
