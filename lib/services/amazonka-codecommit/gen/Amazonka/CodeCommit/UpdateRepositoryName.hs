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
-- Module      : Amazonka.CodeCommit.UpdateRepositoryName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Renames a repository. The repository name must be unique across the
-- calling AWS account. Repository names are limited to 100 alphanumeric,
-- dash, and underscore characters, and cannot include certain characters.
-- The suffix .git is prohibited. For more information about the limits on
-- repository names, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/limits.html Limits>
-- in the AWS CodeCommit User Guide.
module Amazonka.CodeCommit.UpdateRepositoryName
  ( -- * Creating a Request
    UpdateRepositoryName (..),
    newUpdateRepositoryName,

    -- * Request Lenses
    updateRepositoryName_oldName,
    updateRepositoryName_newName,

    -- * Destructuring the Response
    UpdateRepositoryNameResponse (..),
    newUpdateRepositoryNameResponse,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of an update repository description operation.
--
-- /See:/ 'newUpdateRepositoryName' smart constructor.
data UpdateRepositoryName = UpdateRepositoryName'
  { -- | The current name of the repository.
    oldName :: Prelude.Text,
    -- | The new name for the repository.
    newName' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRepositoryName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oldName', 'updateRepositoryName_oldName' - The current name of the repository.
--
-- 'newName'', 'updateRepositoryName_newName' - The new name for the repository.
newUpdateRepositoryName ::
  -- | 'oldName'
  Prelude.Text ->
  -- | 'newName''
  Prelude.Text ->
  UpdateRepositoryName
newUpdateRepositoryName pOldName_ pNewName_ =
  UpdateRepositoryName'
    { oldName = pOldName_,
      newName' = pNewName_
    }

-- | The current name of the repository.
updateRepositoryName_oldName :: Lens.Lens' UpdateRepositoryName Prelude.Text
updateRepositoryName_oldName = Lens.lens (\UpdateRepositoryName' {oldName} -> oldName) (\s@UpdateRepositoryName' {} a -> s {oldName = a} :: UpdateRepositoryName)

-- | The new name for the repository.
updateRepositoryName_newName :: Lens.Lens' UpdateRepositoryName Prelude.Text
updateRepositoryName_newName = Lens.lens (\UpdateRepositoryName' {newName'} -> newName') (\s@UpdateRepositoryName' {} a -> s {newName' = a} :: UpdateRepositoryName)

instance Core.AWSRequest UpdateRepositoryName where
  type
    AWSResponse UpdateRepositoryName =
      UpdateRepositoryNameResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateRepositoryNameResponse'

instance Prelude.Hashable UpdateRepositoryName where
  hashWithSalt _salt UpdateRepositoryName' {..} =
    _salt
      `Prelude.hashWithSalt` oldName
      `Prelude.hashWithSalt` newName'

instance Prelude.NFData UpdateRepositoryName where
  rnf UpdateRepositoryName' {..} =
    Prelude.rnf oldName
      `Prelude.seq` Prelude.rnf newName'

instance Data.ToHeaders UpdateRepositoryName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.UpdateRepositoryName" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRepositoryName where
  toJSON UpdateRepositoryName' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("oldName" Data..= oldName),
            Prelude.Just ("newName" Data..= newName')
          ]
      )

instance Data.ToPath UpdateRepositoryName where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRepositoryName where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRepositoryNameResponse' smart constructor.
data UpdateRepositoryNameResponse = UpdateRepositoryNameResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRepositoryNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateRepositoryNameResponse ::
  UpdateRepositoryNameResponse
newUpdateRepositoryNameResponse =
  UpdateRepositoryNameResponse'

instance Prelude.NFData UpdateRepositoryNameResponse where
  rnf _ = ()
