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
-- Module      : Network.AWS.SageMaker.DeleteTrialComponent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified trial component. A trial component must be
-- disassociated from all trials before the trial component can be deleted.
-- To disassociate a trial component from a trial, call the
-- DisassociateTrialComponent API.
module Network.AWS.SageMaker.DeleteTrialComponent
  ( -- * Creating a Request
    DeleteTrialComponent (..),
    newDeleteTrialComponent,

    -- * Request Lenses
    deleteTrialComponent_trialComponentName,

    -- * Destructuring the Response
    DeleteTrialComponentResponse (..),
    newDeleteTrialComponentResponse,

    -- * Response Lenses
    deleteTrialComponentResponse_trialComponentArn,
    deleteTrialComponentResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteTrialComponent' smart constructor.
data DeleteTrialComponent = DeleteTrialComponent'
  { -- | The name of the component to delete.
    trialComponentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrialComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialComponentName', 'deleteTrialComponent_trialComponentName' - The name of the component to delete.
newDeleteTrialComponent ::
  -- | 'trialComponentName'
  Prelude.Text ->
  DeleteTrialComponent
newDeleteTrialComponent pTrialComponentName_ =
  DeleteTrialComponent'
    { trialComponentName =
        pTrialComponentName_
    }

-- | The name of the component to delete.
deleteTrialComponent_trialComponentName :: Lens.Lens' DeleteTrialComponent Prelude.Text
deleteTrialComponent_trialComponentName = Lens.lens (\DeleteTrialComponent' {trialComponentName} -> trialComponentName) (\s@DeleteTrialComponent' {} a -> s {trialComponentName = a} :: DeleteTrialComponent)

instance Prelude.AWSRequest DeleteTrialComponent where
  type
    Rs DeleteTrialComponent =
      DeleteTrialComponentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTrialComponentResponse'
            Prelude.<$> (x Prelude..?> "TrialComponentArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTrialComponent

instance Prelude.NFData DeleteTrialComponent

instance Prelude.ToHeaders DeleteTrialComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DeleteTrialComponent" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteTrialComponent where
  toJSON DeleteTrialComponent' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "TrialComponentName"
                  Prelude..= trialComponentName
              )
          ]
      )

instance Prelude.ToPath DeleteTrialComponent where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteTrialComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTrialComponentResponse' smart constructor.
data DeleteTrialComponentResponse = DeleteTrialComponentResponse'
  { -- | The Amazon Resource Name (ARN) of the component is being deleted.
    trialComponentArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrialComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialComponentArn', 'deleteTrialComponentResponse_trialComponentArn' - The Amazon Resource Name (ARN) of the component is being deleted.
--
-- 'httpStatus', 'deleteTrialComponentResponse_httpStatus' - The response's http status code.
newDeleteTrialComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTrialComponentResponse
newDeleteTrialComponentResponse pHttpStatus_ =
  DeleteTrialComponentResponse'
    { trialComponentArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the component is being deleted.
deleteTrialComponentResponse_trialComponentArn :: Lens.Lens' DeleteTrialComponentResponse (Prelude.Maybe Prelude.Text)
deleteTrialComponentResponse_trialComponentArn = Lens.lens (\DeleteTrialComponentResponse' {trialComponentArn} -> trialComponentArn) (\s@DeleteTrialComponentResponse' {} a -> s {trialComponentArn = a} :: DeleteTrialComponentResponse)

-- | The response's http status code.
deleteTrialComponentResponse_httpStatus :: Lens.Lens' DeleteTrialComponentResponse Prelude.Int
deleteTrialComponentResponse_httpStatus = Lens.lens (\DeleteTrialComponentResponse' {httpStatus} -> httpStatus) (\s@DeleteTrialComponentResponse' {} a -> s {httpStatus = a} :: DeleteTrialComponentResponse)

instance Prelude.NFData DeleteTrialComponentResponse
