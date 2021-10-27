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
-- Module      : Network.AWS.LookoutEquipment.DeleteInferenceScheduler
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an inference scheduler that has been set up. Already processed
-- output results are not affected.
module Network.AWS.LookoutEquipment.DeleteInferenceScheduler
  ( -- * Creating a Request
    DeleteInferenceScheduler (..),
    newDeleteInferenceScheduler,

    -- * Request Lenses
    deleteInferenceScheduler_inferenceSchedulerName,

    -- * Destructuring the Response
    DeleteInferenceSchedulerResponse (..),
    newDeleteInferenceSchedulerResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LookoutEquipment.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteInferenceScheduler' smart constructor.
data DeleteInferenceScheduler = DeleteInferenceScheduler'
  { -- | The name of the inference scheduler to be deleted.
    inferenceSchedulerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInferenceScheduler' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inferenceSchedulerName', 'deleteInferenceScheduler_inferenceSchedulerName' - The name of the inference scheduler to be deleted.
newDeleteInferenceScheduler ::
  -- | 'inferenceSchedulerName'
  Prelude.Text ->
  DeleteInferenceScheduler
newDeleteInferenceScheduler pInferenceSchedulerName_ =
  DeleteInferenceScheduler'
    { inferenceSchedulerName =
        pInferenceSchedulerName_
    }

-- | The name of the inference scheduler to be deleted.
deleteInferenceScheduler_inferenceSchedulerName :: Lens.Lens' DeleteInferenceScheduler Prelude.Text
deleteInferenceScheduler_inferenceSchedulerName = Lens.lens (\DeleteInferenceScheduler' {inferenceSchedulerName} -> inferenceSchedulerName) (\s@DeleteInferenceScheduler' {} a -> s {inferenceSchedulerName = a} :: DeleteInferenceScheduler)

instance Core.AWSRequest DeleteInferenceScheduler where
  type
    AWSResponse DeleteInferenceScheduler =
      DeleteInferenceSchedulerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteInferenceSchedulerResponse'

instance Prelude.Hashable DeleteInferenceScheduler

instance Prelude.NFData DeleteInferenceScheduler

instance Core.ToHeaders DeleteInferenceScheduler where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLookoutEquipmentFrontendService.DeleteInferenceScheduler" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteInferenceScheduler where
  toJSON DeleteInferenceScheduler' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "InferenceSchedulerName"
                  Core..= inferenceSchedulerName
              )
          ]
      )

instance Core.ToPath DeleteInferenceScheduler where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteInferenceScheduler where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteInferenceSchedulerResponse' smart constructor.
data DeleteInferenceSchedulerResponse = DeleteInferenceSchedulerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInferenceSchedulerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteInferenceSchedulerResponse ::
  DeleteInferenceSchedulerResponse
newDeleteInferenceSchedulerResponse =
  DeleteInferenceSchedulerResponse'

instance
  Prelude.NFData
    DeleteInferenceSchedulerResponse
