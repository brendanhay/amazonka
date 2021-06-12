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
-- Module      : Network.AWS.SageMaker.DeleteAlgorithm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified algorithm from your account.
module Network.AWS.SageMaker.DeleteAlgorithm
  ( -- * Creating a Request
    DeleteAlgorithm (..),
    newDeleteAlgorithm,

    -- * Request Lenses
    deleteAlgorithm_algorithmName,

    -- * Destructuring the Response
    DeleteAlgorithmResponse (..),
    newDeleteAlgorithmResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteAlgorithm' smart constructor.
data DeleteAlgorithm = DeleteAlgorithm'
  { -- | The name of the algorithm to delete.
    algorithmName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAlgorithm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithmName', 'deleteAlgorithm_algorithmName' - The name of the algorithm to delete.
newDeleteAlgorithm ::
  -- | 'algorithmName'
  Core.Text ->
  DeleteAlgorithm
newDeleteAlgorithm pAlgorithmName_ =
  DeleteAlgorithm' {algorithmName = pAlgorithmName_}

-- | The name of the algorithm to delete.
deleteAlgorithm_algorithmName :: Lens.Lens' DeleteAlgorithm Core.Text
deleteAlgorithm_algorithmName = Lens.lens (\DeleteAlgorithm' {algorithmName} -> algorithmName) (\s@DeleteAlgorithm' {} a -> s {algorithmName = a} :: DeleteAlgorithm)

instance Core.AWSRequest DeleteAlgorithm where
  type
    AWSResponse DeleteAlgorithm =
      DeleteAlgorithmResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteAlgorithmResponse'

instance Core.Hashable DeleteAlgorithm

instance Core.NFData DeleteAlgorithm

instance Core.ToHeaders DeleteAlgorithm where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DeleteAlgorithm" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteAlgorithm where
  toJSON DeleteAlgorithm' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("AlgorithmName" Core..= algorithmName)]
      )

instance Core.ToPath DeleteAlgorithm where
  toPath = Core.const "/"

instance Core.ToQuery DeleteAlgorithm where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAlgorithmResponse' smart constructor.
data DeleteAlgorithmResponse = DeleteAlgorithmResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAlgorithmResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAlgorithmResponse ::
  DeleteAlgorithmResponse
newDeleteAlgorithmResponse = DeleteAlgorithmResponse'

instance Core.NFData DeleteAlgorithmResponse
