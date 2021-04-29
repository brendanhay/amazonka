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
-- Module      : Network.AWS.SageMaker.DeleteCodeRepository
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Git repository from your account.
module Network.AWS.SageMaker.DeleteCodeRepository
  ( -- * Creating a Request
    DeleteCodeRepository (..),
    newDeleteCodeRepository,

    -- * Request Lenses
    deleteCodeRepository_codeRepositoryName,

    -- * Destructuring the Response
    DeleteCodeRepositoryResponse (..),
    newDeleteCodeRepositoryResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteCodeRepository' smart constructor.
data DeleteCodeRepository = DeleteCodeRepository'
  { -- | The name of the Git repository to delete.
    codeRepositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteCodeRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeRepositoryName', 'deleteCodeRepository_codeRepositoryName' - The name of the Git repository to delete.
newDeleteCodeRepository ::
  -- | 'codeRepositoryName'
  Prelude.Text ->
  DeleteCodeRepository
newDeleteCodeRepository pCodeRepositoryName_ =
  DeleteCodeRepository'
    { codeRepositoryName =
        pCodeRepositoryName_
    }

-- | The name of the Git repository to delete.
deleteCodeRepository_codeRepositoryName :: Lens.Lens' DeleteCodeRepository Prelude.Text
deleteCodeRepository_codeRepositoryName = Lens.lens (\DeleteCodeRepository' {codeRepositoryName} -> codeRepositoryName) (\s@DeleteCodeRepository' {} a -> s {codeRepositoryName = a} :: DeleteCodeRepository)

instance Prelude.AWSRequest DeleteCodeRepository where
  type
    Rs DeleteCodeRepository =
      DeleteCodeRepositoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteCodeRepositoryResponse'

instance Prelude.Hashable DeleteCodeRepository

instance Prelude.NFData DeleteCodeRepository

instance Prelude.ToHeaders DeleteCodeRepository where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DeleteCodeRepository" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteCodeRepository where
  toJSON DeleteCodeRepository' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CodeRepositoryName"
                  Prelude..= codeRepositoryName
              )
          ]
      )

instance Prelude.ToPath DeleteCodeRepository where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteCodeRepository where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCodeRepositoryResponse' smart constructor.
data DeleteCodeRepositoryResponse = DeleteCodeRepositoryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteCodeRepositoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCodeRepositoryResponse ::
  DeleteCodeRepositoryResponse
newDeleteCodeRepositoryResponse =
  DeleteCodeRepositoryResponse'

instance Prelude.NFData DeleteCodeRepositoryResponse
