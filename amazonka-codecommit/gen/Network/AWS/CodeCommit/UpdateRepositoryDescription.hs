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
-- Module      : Network.AWS.CodeCommit.UpdateRepositoryDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.CodeCommit.UpdateRepositoryDescription
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

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.AWSRequest
    UpdateRepositoryDescription
  where
  type
    Rs UpdateRepositoryDescription =
      UpdateRepositoryDescriptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateRepositoryDescriptionResponse'

instance Prelude.Hashable UpdateRepositoryDescription

instance Prelude.NFData UpdateRepositoryDescription

instance
  Prelude.ToHeaders
    UpdateRepositoryDescription
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.UpdateRepositoryDescription" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateRepositoryDescription where
  toJSON UpdateRepositoryDescription' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("repositoryDescription" Prelude..=)
              Prelude.<$> repositoryDescription,
            Prelude.Just
              ("repositoryName" Prelude..= repositoryName)
          ]
      )

instance Prelude.ToPath UpdateRepositoryDescription where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateRepositoryDescription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRepositoryDescriptionResponse' smart constructor.
data UpdateRepositoryDescriptionResponse = UpdateRepositoryDescriptionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
