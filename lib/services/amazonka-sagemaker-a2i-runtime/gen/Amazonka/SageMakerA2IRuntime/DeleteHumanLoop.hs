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
-- Module      : Amazonka.SageMakerA2IRuntime.DeleteHumanLoop
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified human loop for a flow definition.
--
-- If the human loop was deleted, this operation will return a
-- @ResourceNotFoundException@.
module Amazonka.SageMakerA2IRuntime.DeleteHumanLoop
  ( -- * Creating a Request
    DeleteHumanLoop (..),
    newDeleteHumanLoop,

    -- * Request Lenses
    deleteHumanLoop_humanLoopName,

    -- * Destructuring the Response
    DeleteHumanLoopResponse (..),
    newDeleteHumanLoopResponse,

    -- * Response Lenses
    deleteHumanLoopResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerA2IRuntime.Types

-- | /See:/ 'newDeleteHumanLoop' smart constructor.
data DeleteHumanLoop = DeleteHumanLoop'
  { -- | The name of the human loop that you want to delete.
    humanLoopName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHumanLoop' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'humanLoopName', 'deleteHumanLoop_humanLoopName' - The name of the human loop that you want to delete.
newDeleteHumanLoop ::
  -- | 'humanLoopName'
  Prelude.Text ->
  DeleteHumanLoop
newDeleteHumanLoop pHumanLoopName_ =
  DeleteHumanLoop' {humanLoopName = pHumanLoopName_}

-- | The name of the human loop that you want to delete.
deleteHumanLoop_humanLoopName :: Lens.Lens' DeleteHumanLoop Prelude.Text
deleteHumanLoop_humanLoopName = Lens.lens (\DeleteHumanLoop' {humanLoopName} -> humanLoopName) (\s@DeleteHumanLoop' {} a -> s {humanLoopName = a} :: DeleteHumanLoop)

instance Core.AWSRequest DeleteHumanLoop where
  type
    AWSResponse DeleteHumanLoop =
      DeleteHumanLoopResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteHumanLoopResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteHumanLoop where
  hashWithSalt _salt DeleteHumanLoop' {..} =
    _salt `Prelude.hashWithSalt` humanLoopName

instance Prelude.NFData DeleteHumanLoop where
  rnf DeleteHumanLoop' {..} = Prelude.rnf humanLoopName

instance Data.ToHeaders DeleteHumanLoop where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteHumanLoop where
  toPath DeleteHumanLoop' {..} =
    Prelude.mconcat
      ["/human-loops/", Data.toBS humanLoopName]

instance Data.ToQuery DeleteHumanLoop where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteHumanLoopResponse' smart constructor.
data DeleteHumanLoopResponse = DeleteHumanLoopResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHumanLoopResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteHumanLoopResponse_httpStatus' - The response's http status code.
newDeleteHumanLoopResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteHumanLoopResponse
newDeleteHumanLoopResponse pHttpStatus_ =
  DeleteHumanLoopResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteHumanLoopResponse_httpStatus :: Lens.Lens' DeleteHumanLoopResponse Prelude.Int
deleteHumanLoopResponse_httpStatus = Lens.lens (\DeleteHumanLoopResponse' {httpStatus} -> httpStatus) (\s@DeleteHumanLoopResponse' {} a -> s {httpStatus = a} :: DeleteHumanLoopResponse)

instance Prelude.NFData DeleteHumanLoopResponse where
  rnf DeleteHumanLoopResponse' {..} =
    Prelude.rnf httpStatus
