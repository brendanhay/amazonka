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
-- Module      : Network.AWS.SageMaker.DeleteDataQualityJobDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a data quality monitoring job definition.
module Network.AWS.SageMaker.DeleteDataQualityJobDefinition
  ( -- * Creating a Request
    DeleteDataQualityJobDefinition (..),
    newDeleteDataQualityJobDefinition,

    -- * Request Lenses
    deleteDataQualityJobDefinition_jobDefinitionName,

    -- * Destructuring the Response
    DeleteDataQualityJobDefinitionResponse (..),
    newDeleteDataQualityJobDefinitionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteDataQualityJobDefinition' smart constructor.
data DeleteDataQualityJobDefinition = DeleteDataQualityJobDefinition'
  { -- | The name of the data quality monitoring job definition to delete.
    jobDefinitionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataQualityJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobDefinitionName', 'deleteDataQualityJobDefinition_jobDefinitionName' - The name of the data quality monitoring job definition to delete.
newDeleteDataQualityJobDefinition ::
  -- | 'jobDefinitionName'
  Prelude.Text ->
  DeleteDataQualityJobDefinition
newDeleteDataQualityJobDefinition pJobDefinitionName_ =
  DeleteDataQualityJobDefinition'
    { jobDefinitionName =
        pJobDefinitionName_
    }

-- | The name of the data quality monitoring job definition to delete.
deleteDataQualityJobDefinition_jobDefinitionName :: Lens.Lens' DeleteDataQualityJobDefinition Prelude.Text
deleteDataQualityJobDefinition_jobDefinitionName = Lens.lens (\DeleteDataQualityJobDefinition' {jobDefinitionName} -> jobDefinitionName) (\s@DeleteDataQualityJobDefinition' {} a -> s {jobDefinitionName = a} :: DeleteDataQualityJobDefinition)

instance
  Prelude.AWSRequest
    DeleteDataQualityJobDefinition
  where
  type
    Rs DeleteDataQualityJobDefinition =
      DeleteDataQualityJobDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteDataQualityJobDefinitionResponse'

instance
  Prelude.Hashable
    DeleteDataQualityJobDefinition

instance
  Prelude.NFData
    DeleteDataQualityJobDefinition

instance
  Prelude.ToHeaders
    DeleteDataQualityJobDefinition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DeleteDataQualityJobDefinition" ::
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
    DeleteDataQualityJobDefinition
  where
  toJSON DeleteDataQualityJobDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("JobDefinitionName" Prelude..= jobDefinitionName)
          ]
      )

instance
  Prelude.ToPath
    DeleteDataQualityJobDefinition
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteDataQualityJobDefinition
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDataQualityJobDefinitionResponse' smart constructor.
data DeleteDataQualityJobDefinitionResponse = DeleteDataQualityJobDefinitionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataQualityJobDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDataQualityJobDefinitionResponse ::
  DeleteDataQualityJobDefinitionResponse
newDeleteDataQualityJobDefinitionResponse =
  DeleteDataQualityJobDefinitionResponse'

instance
  Prelude.NFData
    DeleteDataQualityJobDefinitionResponse
