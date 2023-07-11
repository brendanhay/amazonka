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
-- Module      : Amazonka.SageMaker.DeleteModelQualityJobDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the secified model quality monitoring job definition.
module Amazonka.SageMaker.DeleteModelQualityJobDefinition
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteModelQualityJobDefinition' smart constructor.
data DeleteModelQualityJobDefinition = DeleteModelQualityJobDefinition'
  { -- | The name of the model quality monitoring job definition to delete.
    jobDefinitionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    DeleteModelQualityJobDefinition
  where
  type
    AWSResponse DeleteModelQualityJobDefinition =
      DeleteModelQualityJobDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteModelQualityJobDefinitionResponse'

instance
  Prelude.Hashable
    DeleteModelQualityJobDefinition
  where
  hashWithSalt
    _salt
    DeleteModelQualityJobDefinition' {..} =
      _salt `Prelude.hashWithSalt` jobDefinitionName

instance
  Prelude.NFData
    DeleteModelQualityJobDefinition
  where
  rnf DeleteModelQualityJobDefinition' {..} =
    Prelude.rnf jobDefinitionName

instance
  Data.ToHeaders
    DeleteModelQualityJobDefinition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DeleteModelQualityJobDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteModelQualityJobDefinition where
  toJSON DeleteModelQualityJobDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("JobDefinitionName" Data..= jobDefinitionName)
          ]
      )

instance Data.ToPath DeleteModelQualityJobDefinition where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteModelQualityJobDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteModelQualityJobDefinitionResponse' smart constructor.
data DeleteModelQualityJobDefinitionResponse = DeleteModelQualityJobDefinitionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
