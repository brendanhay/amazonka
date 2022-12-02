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
-- Module      : Amazonka.SageMaker.DeleteCodeRepository
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Git repository from your account.
module Amazonka.SageMaker.DeleteCodeRepository
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteCodeRepository' smart constructor.
data DeleteCodeRepository = DeleteCodeRepository'
  { -- | The name of the Git repository to delete.
    codeRepositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteCodeRepository where
  type
    AWSResponse DeleteCodeRepository =
      DeleteCodeRepositoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteCodeRepositoryResponse'

instance Prelude.Hashable DeleteCodeRepository where
  hashWithSalt _salt DeleteCodeRepository' {..} =
    _salt `Prelude.hashWithSalt` codeRepositoryName

instance Prelude.NFData DeleteCodeRepository where
  rnf DeleteCodeRepository' {..} =
    Prelude.rnf codeRepositoryName

instance Data.ToHeaders DeleteCodeRepository where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DeleteCodeRepository" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteCodeRepository where
  toJSON DeleteCodeRepository' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CodeRepositoryName" Data..= codeRepositoryName)
          ]
      )

instance Data.ToPath DeleteCodeRepository where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteCodeRepository where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCodeRepositoryResponse' smart constructor.
data DeleteCodeRepositoryResponse = DeleteCodeRepositoryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCodeRepositoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCodeRepositoryResponse ::
  DeleteCodeRepositoryResponse
newDeleteCodeRepositoryResponse =
  DeleteCodeRepositoryResponse'

instance Prelude.NFData DeleteCodeRepositoryResponse where
  rnf _ = ()
