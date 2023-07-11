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
-- Module      : Amazonka.CodeCommit.UpdateRepositoryDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets or changes the comment or description for a repository.
--
-- The description field for a repository accepts all HTML characters and
-- all valid Unicode characters. Applications that do not HTML-encode the
-- description and display it in a webpage can expose users to potentially
-- malicious code. Make sure that you HTML-encode the description field in
-- any application that uses this API to display the repository description
-- on a webpage.
module Amazonka.CodeCommit.UpdateRepositoryDescription
  ( -- * Creating a Request
    UpdateRepositoryDescription (..),
    newUpdateRepositoryDescription,

    -- * Request Lenses
    updateRepositoryDescription_repositoryDescription,
    updateRepositoryDescription_repositoryName,

    -- * Destructuring the Response
    UpdateRepositoryDescriptionResponse (..),
    newUpdateRepositoryDescriptionResponse,
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
-- /See:/ 'newUpdateRepositoryDescription' smart constructor.
data UpdateRepositoryDescription = UpdateRepositoryDescription'
  { -- | The new comment or description for the specified repository. Repository
    -- descriptions are limited to 1,000 characters.
    repositoryDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository to set or change the comment or description
    -- for.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRepositoryDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryDescription', 'updateRepositoryDescription_repositoryDescription' - The new comment or description for the specified repository. Repository
-- descriptions are limited to 1,000 characters.
--
-- 'repositoryName', 'updateRepositoryDescription_repositoryName' - The name of the repository to set or change the comment or description
-- for.
newUpdateRepositoryDescription ::
  -- | 'repositoryName'
  Prelude.Text ->
  UpdateRepositoryDescription
newUpdateRepositoryDescription pRepositoryName_ =
  UpdateRepositoryDescription'
    { repositoryDescription =
        Prelude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The new comment or description for the specified repository. Repository
-- descriptions are limited to 1,000 characters.
updateRepositoryDescription_repositoryDescription :: Lens.Lens' UpdateRepositoryDescription (Prelude.Maybe Prelude.Text)
updateRepositoryDescription_repositoryDescription = Lens.lens (\UpdateRepositoryDescription' {repositoryDescription} -> repositoryDescription) (\s@UpdateRepositoryDescription' {} a -> s {repositoryDescription = a} :: UpdateRepositoryDescription)

-- | The name of the repository to set or change the comment or description
-- for.
updateRepositoryDescription_repositoryName :: Lens.Lens' UpdateRepositoryDescription Prelude.Text
updateRepositoryDescription_repositoryName = Lens.lens (\UpdateRepositoryDescription' {repositoryName} -> repositoryName) (\s@UpdateRepositoryDescription' {} a -> s {repositoryName = a} :: UpdateRepositoryDescription)

instance Core.AWSRequest UpdateRepositoryDescription where
  type
    AWSResponse UpdateRepositoryDescription =
      UpdateRepositoryDescriptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateRepositoryDescriptionResponse'

instance Prelude.Hashable UpdateRepositoryDescription where
  hashWithSalt _salt UpdateRepositoryDescription' {..} =
    _salt
      `Prelude.hashWithSalt` repositoryDescription
      `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData UpdateRepositoryDescription where
  rnf UpdateRepositoryDescription' {..} =
    Prelude.rnf repositoryDescription
      `Prelude.seq` Prelude.rnf repositoryName

instance Data.ToHeaders UpdateRepositoryDescription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.UpdateRepositoryDescription" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRepositoryDescription where
  toJSON UpdateRepositoryDescription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("repositoryDescription" Data..=)
              Prelude.<$> repositoryDescription,
            Prelude.Just
              ("repositoryName" Data..= repositoryName)
          ]
      )

instance Data.ToPath UpdateRepositoryDescription where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRepositoryDescription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRepositoryDescriptionResponse' smart constructor.
data UpdateRepositoryDescriptionResponse = UpdateRepositoryDescriptionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRepositoryDescriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateRepositoryDescriptionResponse ::
  UpdateRepositoryDescriptionResponse
newUpdateRepositoryDescriptionResponse =
  UpdateRepositoryDescriptionResponse'

instance
  Prelude.NFData
    UpdateRepositoryDescriptionResponse
  where
  rnf _ = ()
