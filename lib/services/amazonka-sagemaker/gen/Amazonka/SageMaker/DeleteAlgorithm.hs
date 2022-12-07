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
-- Module      : Amazonka.SageMaker.DeleteAlgorithm
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified algorithm from your account.
module Amazonka.SageMaker.DeleteAlgorithm
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteAlgorithm' smart constructor.
data DeleteAlgorithm = DeleteAlgorithm'
  { -- | The name of the algorithm to delete.
    algorithmName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteAlgorithm
newDeleteAlgorithm pAlgorithmName_ =
  DeleteAlgorithm' {algorithmName = pAlgorithmName_}

-- | The name of the algorithm to delete.
deleteAlgorithm_algorithmName :: Lens.Lens' DeleteAlgorithm Prelude.Text
deleteAlgorithm_algorithmName = Lens.lens (\DeleteAlgorithm' {algorithmName} -> algorithmName) (\s@DeleteAlgorithm' {} a -> s {algorithmName = a} :: DeleteAlgorithm)

instance Core.AWSRequest DeleteAlgorithm where
  type
    AWSResponse DeleteAlgorithm =
      DeleteAlgorithmResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteAlgorithmResponse'

instance Prelude.Hashable DeleteAlgorithm where
  hashWithSalt _salt DeleteAlgorithm' {..} =
    _salt `Prelude.hashWithSalt` algorithmName

instance Prelude.NFData DeleteAlgorithm where
  rnf DeleteAlgorithm' {..} = Prelude.rnf algorithmName

instance Data.ToHeaders DeleteAlgorithm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DeleteAlgorithm" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAlgorithm where
  toJSON DeleteAlgorithm' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AlgorithmName" Data..= algorithmName)
          ]
      )

instance Data.ToPath DeleteAlgorithm where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAlgorithm where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAlgorithmResponse' smart constructor.
data DeleteAlgorithmResponse = DeleteAlgorithmResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAlgorithmResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAlgorithmResponse ::
  DeleteAlgorithmResponse
newDeleteAlgorithmResponse = DeleteAlgorithmResponse'

instance Prelude.NFData DeleteAlgorithmResponse where
  rnf _ = ()
