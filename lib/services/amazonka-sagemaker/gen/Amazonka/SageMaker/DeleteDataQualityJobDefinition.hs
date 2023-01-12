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
-- Module      : Amazonka.SageMaker.DeleteDataQualityJobDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a data quality monitoring job definition.
module Amazonka.SageMaker.DeleteDataQualityJobDefinition
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteDataQualityJobDefinition' smart constructor.
data DeleteDataQualityJobDefinition = DeleteDataQualityJobDefinition'
  { -- | The name of the data quality monitoring job definition to delete.
    jobDefinitionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    DeleteDataQualityJobDefinition
  where
  type
    AWSResponse DeleteDataQualityJobDefinition =
      DeleteDataQualityJobDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteDataQualityJobDefinitionResponse'

instance
  Prelude.Hashable
    DeleteDataQualityJobDefinition
  where
  hashWithSalt
    _salt
    DeleteDataQualityJobDefinition' {..} =
      _salt `Prelude.hashWithSalt` jobDefinitionName

instance
  Prelude.NFData
    DeleteDataQualityJobDefinition
  where
  rnf DeleteDataQualityJobDefinition' {..} =
    Prelude.rnf jobDefinitionName

instance
  Data.ToHeaders
    DeleteDataQualityJobDefinition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DeleteDataQualityJobDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDataQualityJobDefinition where
  toJSON DeleteDataQualityJobDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("JobDefinitionName" Data..= jobDefinitionName)
          ]
      )

instance Data.ToPath DeleteDataQualityJobDefinition where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDataQualityJobDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDataQualityJobDefinitionResponse' smart constructor.
data DeleteDataQualityJobDefinitionResponse = DeleteDataQualityJobDefinitionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
