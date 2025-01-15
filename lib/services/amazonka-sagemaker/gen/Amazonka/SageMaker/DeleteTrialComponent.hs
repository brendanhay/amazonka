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
-- Module      : Amazonka.SageMaker.DeleteTrialComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified trial component. A trial component must be
-- disassociated from all trials before the trial component can be deleted.
-- To disassociate a trial component from a trial, call the
-- DisassociateTrialComponent API.
module Amazonka.SageMaker.DeleteTrialComponent
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteTrialComponent' smart constructor.
data DeleteTrialComponent = DeleteTrialComponent'
  { -- | The name of the component to delete.
    trialComponentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteTrialComponent where
  type
    AWSResponse DeleteTrialComponent =
      DeleteTrialComponentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTrialComponentResponse'
            Prelude.<$> (x Data..?> "TrialComponentArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTrialComponent where
  hashWithSalt _salt DeleteTrialComponent' {..} =
    _salt `Prelude.hashWithSalt` trialComponentName

instance Prelude.NFData DeleteTrialComponent where
  rnf DeleteTrialComponent' {..} =
    Prelude.rnf trialComponentName

instance Data.ToHeaders DeleteTrialComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DeleteTrialComponent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteTrialComponent where
  toJSON DeleteTrialComponent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TrialComponentName" Data..= trialComponentName)
          ]
      )

instance Data.ToPath DeleteTrialComponent where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTrialComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTrialComponentResponse' smart constructor.
data DeleteTrialComponentResponse = DeleteTrialComponentResponse'
  { -- | The Amazon Resource Name (ARN) of the component is being deleted.
    trialComponentArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteTrialComponentResponse where
  rnf DeleteTrialComponentResponse' {..} =
    Prelude.rnf trialComponentArn `Prelude.seq`
      Prelude.rnf httpStatus
