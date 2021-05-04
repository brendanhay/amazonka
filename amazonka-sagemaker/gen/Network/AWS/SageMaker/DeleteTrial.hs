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
-- Module      : Network.AWS.SageMaker.DeleteTrial
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified trial. All trial components that make up the trial
-- must be deleted first. Use the DescribeTrialComponent API to get the
-- list of trial components.
module Network.AWS.SageMaker.DeleteTrial
  ( -- * Creating a Request
    DeleteTrial (..),
    newDeleteTrial,

    -- * Request Lenses
    deleteTrial_trialName,

    -- * Destructuring the Response
    DeleteTrialResponse (..),
    newDeleteTrialResponse,

    -- * Response Lenses
    deleteTrialResponse_trialArn,
    deleteTrialResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteTrial' smart constructor.
data DeleteTrial = DeleteTrial'
  { -- | The name of the trial to delete.
    trialName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrial' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialName', 'deleteTrial_trialName' - The name of the trial to delete.
newDeleteTrial ::
  -- | 'trialName'
  Prelude.Text ->
  DeleteTrial
newDeleteTrial pTrialName_ =
  DeleteTrial' {trialName = pTrialName_}

-- | The name of the trial to delete.
deleteTrial_trialName :: Lens.Lens' DeleteTrial Prelude.Text
deleteTrial_trialName = Lens.lens (\DeleteTrial' {trialName} -> trialName) (\s@DeleteTrial' {} a -> s {trialName = a} :: DeleteTrial)

instance Prelude.AWSRequest DeleteTrial where
  type Rs DeleteTrial = DeleteTrialResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTrialResponse'
            Prelude.<$> (x Prelude..?> "TrialArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTrial

instance Prelude.NFData DeleteTrial

instance Prelude.ToHeaders DeleteTrial where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.DeleteTrial" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteTrial where
  toJSON DeleteTrial' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("TrialName" Prelude..= trialName)]
      )

instance Prelude.ToPath DeleteTrial where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteTrial where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTrialResponse' smart constructor.
data DeleteTrialResponse = DeleteTrialResponse'
  { -- | The Amazon Resource Name (ARN) of the trial that is being deleted.
    trialArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrialResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialArn', 'deleteTrialResponse_trialArn' - The Amazon Resource Name (ARN) of the trial that is being deleted.
--
-- 'httpStatus', 'deleteTrialResponse_httpStatus' - The response's http status code.
newDeleteTrialResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTrialResponse
newDeleteTrialResponse pHttpStatus_ =
  DeleteTrialResponse'
    { trialArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial that is being deleted.
deleteTrialResponse_trialArn :: Lens.Lens' DeleteTrialResponse (Prelude.Maybe Prelude.Text)
deleteTrialResponse_trialArn = Lens.lens (\DeleteTrialResponse' {trialArn} -> trialArn) (\s@DeleteTrialResponse' {} a -> s {trialArn = a} :: DeleteTrialResponse)

-- | The response's http status code.
deleteTrialResponse_httpStatus :: Lens.Lens' DeleteTrialResponse Prelude.Int
deleteTrialResponse_httpStatus = Lens.lens (\DeleteTrialResponse' {httpStatus} -> httpStatus) (\s@DeleteTrialResponse' {} a -> s {httpStatus = a} :: DeleteTrialResponse)

instance Prelude.NFData DeleteTrialResponse
