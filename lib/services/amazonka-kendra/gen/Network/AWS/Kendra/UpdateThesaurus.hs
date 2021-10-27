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
-- Module      : Network.AWS.Kendra.UpdateThesaurus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a thesaurus file associated with an index.
module Network.AWS.Kendra.UpdateThesaurus
  ( -- * Creating a Request
    UpdateThesaurus (..),
    newUpdateThesaurus,

    -- * Request Lenses
    updateThesaurus_sourceS3Path,
    updateThesaurus_name,
    updateThesaurus_description,
    updateThesaurus_roleArn,
    updateThesaurus_id,
    updateThesaurus_indexId,

    -- * Destructuring the Response
    UpdateThesaurusResponse (..),
    newUpdateThesaurusResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateThesaurus' smart constructor.
data UpdateThesaurus = UpdateThesaurus'
  { sourceS3Path :: Prelude.Maybe S3Path,
    -- | The updated name of the thesaurus.
    name :: Prelude.Maybe Prelude.Text,
    -- | The updated description of the thesaurus.
    description :: Prelude.Maybe Prelude.Text,
    -- | The updated role ARN of the thesaurus.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the thesaurus to update.
    id :: Prelude.Text,
    -- | The identifier of the index associated with the thesaurus to update.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateThesaurus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceS3Path', 'updateThesaurus_sourceS3Path' - Undocumented member.
--
-- 'name', 'updateThesaurus_name' - The updated name of the thesaurus.
--
-- 'description', 'updateThesaurus_description' - The updated description of the thesaurus.
--
-- 'roleArn', 'updateThesaurus_roleArn' - The updated role ARN of the thesaurus.
--
-- 'id', 'updateThesaurus_id' - The identifier of the thesaurus to update.
--
-- 'indexId', 'updateThesaurus_indexId' - The identifier of the index associated with the thesaurus to update.
newUpdateThesaurus ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  UpdateThesaurus
newUpdateThesaurus pId_ pIndexId_ =
  UpdateThesaurus'
    { sourceS3Path = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      id = pId_,
      indexId = pIndexId_
    }

-- | Undocumented member.
updateThesaurus_sourceS3Path :: Lens.Lens' UpdateThesaurus (Prelude.Maybe S3Path)
updateThesaurus_sourceS3Path = Lens.lens (\UpdateThesaurus' {sourceS3Path} -> sourceS3Path) (\s@UpdateThesaurus' {} a -> s {sourceS3Path = a} :: UpdateThesaurus)

-- | The updated name of the thesaurus.
updateThesaurus_name :: Lens.Lens' UpdateThesaurus (Prelude.Maybe Prelude.Text)
updateThesaurus_name = Lens.lens (\UpdateThesaurus' {name} -> name) (\s@UpdateThesaurus' {} a -> s {name = a} :: UpdateThesaurus)

-- | The updated description of the thesaurus.
updateThesaurus_description :: Lens.Lens' UpdateThesaurus (Prelude.Maybe Prelude.Text)
updateThesaurus_description = Lens.lens (\UpdateThesaurus' {description} -> description) (\s@UpdateThesaurus' {} a -> s {description = a} :: UpdateThesaurus)

-- | The updated role ARN of the thesaurus.
updateThesaurus_roleArn :: Lens.Lens' UpdateThesaurus (Prelude.Maybe Prelude.Text)
updateThesaurus_roleArn = Lens.lens (\UpdateThesaurus' {roleArn} -> roleArn) (\s@UpdateThesaurus' {} a -> s {roleArn = a} :: UpdateThesaurus)

-- | The identifier of the thesaurus to update.
updateThesaurus_id :: Lens.Lens' UpdateThesaurus Prelude.Text
updateThesaurus_id = Lens.lens (\UpdateThesaurus' {id} -> id) (\s@UpdateThesaurus' {} a -> s {id = a} :: UpdateThesaurus)

-- | The identifier of the index associated with the thesaurus to update.
updateThesaurus_indexId :: Lens.Lens' UpdateThesaurus Prelude.Text
updateThesaurus_indexId = Lens.lens (\UpdateThesaurus' {indexId} -> indexId) (\s@UpdateThesaurus' {} a -> s {indexId = a} :: UpdateThesaurus)

instance Core.AWSRequest UpdateThesaurus where
  type
    AWSResponse UpdateThesaurus =
      UpdateThesaurusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateThesaurusResponse'

instance Prelude.Hashable UpdateThesaurus

instance Prelude.NFData UpdateThesaurus

instance Core.ToHeaders UpdateThesaurus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.UpdateThesaurus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateThesaurus where
  toJSON UpdateThesaurus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SourceS3Path" Core..=) Prelude.<$> sourceS3Path,
            ("Name" Core..=) Prelude.<$> name,
            ("Description" Core..=) Prelude.<$> description,
            ("RoleArn" Core..=) Prelude.<$> roleArn,
            Prelude.Just ("Id" Core..= id),
            Prelude.Just ("IndexId" Core..= indexId)
          ]
      )

instance Core.ToPath UpdateThesaurus where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateThesaurus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateThesaurusResponse' smart constructor.
data UpdateThesaurusResponse = UpdateThesaurusResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateThesaurusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateThesaurusResponse ::
  UpdateThesaurusResponse
newUpdateThesaurusResponse = UpdateThesaurusResponse'

instance Prelude.NFData UpdateThesaurusResponse
