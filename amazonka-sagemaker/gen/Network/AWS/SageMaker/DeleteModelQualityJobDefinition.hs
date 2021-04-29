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
-- Module      : Network.AWS.SageMaker.DeleteModelQualityJobDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the secified model quality monitoring job definition.
module Network.AWS.SageMaker.DeleteModelQualityJobDefinition
  ( -- * Creating a Request
    DeleteModelQualityJobDefinition (..),
    newDeleteModelQualityJobDefinition,

    -- * Request Lenses
    deleteModelQualityJobDefinition_jobDefinitionName,

    -- * Destructuring the Response
    DeleteModelQualityJobDefinitionResponse (..),
    newDeleteModelQualityJobDefinitionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteModelQualityJobDefinition' smart constructor.
data DeleteModelQualityJobDefinition = DeleteModelQualityJobDefinition'
  { -- | The name of the model quality monitoring job definition to delete.
    jobDefinitionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelQualityJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobDefinitionName', 'deleteModelQualityJobDefinition_jobDefinitionName' - The name of the model quality monitoring job definition to delete.
newDeleteModelQualityJobDefinition ::
  -- | 'jobDefinitionName'
  Prelude.Text ->
  DeleteModelQualityJobDefinition
newDeleteModelQualityJobDefinition
  pJobDefinitionName_ =
    DeleteModelQualityJobDefinition'
      { jobDefinitionName =
          pJobDefinitionName_
      }

-- | The name of the model quality monitoring job definition to delete.
deleteModelQualityJobDefinition_jobDefinitionName :: Lens.Lens' DeleteModelQualityJobDefinition Prelude.Text
deleteModelQualityJobDefinition_jobDefinitionName = Lens.lens (\DeleteModelQualityJobDefinition' {jobDefinitionName} -> jobDefinitionName) (\s@DeleteModelQualityJobDefinition' {} a -> s {jobDefinitionName = a} :: DeleteModelQualityJobDefinition)

instance
  Prelude.AWSRequest
    DeleteModelQualityJobDefinition
  where
  type
    Rs DeleteModelQualityJobDefinition =
      DeleteModelQualityJobDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteModelQualityJobDefinitionResponse'

instance
  Prelude.Hashable
    DeleteModelQualityJobDefinition

instance
  Prelude.NFData
    DeleteModelQualityJobDefinition

instance
  Prelude.ToHeaders
    DeleteModelQualityJobDefinition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DeleteModelQualityJobDefinition" ::
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
    DeleteModelQualityJobDefinition
  where
  toJSON DeleteModelQualityJobDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("JobDefinitionName" Prelude..= jobDefinitionName)
          ]
      )

instance
  Prelude.ToPath
    DeleteModelQualityJobDefinition
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteModelQualityJobDefinition
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteModelQualityJobDefinitionResponse' smart constructor.
data DeleteModelQualityJobDefinitionResponse = DeleteModelQualityJobDefinitionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelQualityJobDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteModelQualityJobDefinitionResponse ::
  DeleteModelQualityJobDefinitionResponse
newDeleteModelQualityJobDefinitionResponse =
  DeleteModelQualityJobDefinitionResponse'

instance
  Prelude.NFData
    DeleteModelQualityJobDefinitionResponse
