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
-- Module      : Amazonka.Kendra.UpdateThesaurus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a thesaurus for an index.
module Amazonka.Kendra.UpdateThesaurus
  ( -- * Creating a Request
    UpdateThesaurus (..),
    newUpdateThesaurus,

    -- * Request Lenses
    updateThesaurus_description,
    updateThesaurus_name,
    updateThesaurus_roleArn,
    updateThesaurus_sourceS3Path,
    updateThesaurus_id,
    updateThesaurus_indexId,

    -- * Destructuring the Response
    UpdateThesaurusResponse (..),
    newUpdateThesaurusResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateThesaurus' smart constructor.
data UpdateThesaurus = UpdateThesaurus'
  { -- | A new description for the thesaurus.
    description :: Prelude.Maybe Prelude.Text,
    -- | A new name for the thesaurus.
    name :: Prelude.Maybe Prelude.Text,
    -- | An IAM role that gives Amazon Kendra permissions to access thesaurus
    -- file specified in @SourceS3Path@.
    roleArn :: Prelude.Maybe Prelude.Text,
    sourceS3Path :: Prelude.Maybe S3Path,
    -- | The identifier of the thesaurus you want to update.
    id :: Prelude.Text,
    -- | The identifier of the index for the thesaurus.
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
-- 'description', 'updateThesaurus_description' - A new description for the thesaurus.
--
-- 'name', 'updateThesaurus_name' - A new name for the thesaurus.
--
-- 'roleArn', 'updateThesaurus_roleArn' - An IAM role that gives Amazon Kendra permissions to access thesaurus
-- file specified in @SourceS3Path@.
--
-- 'sourceS3Path', 'updateThesaurus_sourceS3Path' - Undocumented member.
--
-- 'id', 'updateThesaurus_id' - The identifier of the thesaurus you want to update.
--
-- 'indexId', 'updateThesaurus_indexId' - The identifier of the index for the thesaurus.
newUpdateThesaurus ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  UpdateThesaurus
newUpdateThesaurus pId_ pIndexId_ =
  UpdateThesaurus'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      sourceS3Path = Prelude.Nothing,
      id = pId_,
      indexId = pIndexId_
    }

-- | A new description for the thesaurus.
updateThesaurus_description :: Lens.Lens' UpdateThesaurus (Prelude.Maybe Prelude.Text)
updateThesaurus_description = Lens.lens (\UpdateThesaurus' {description} -> description) (\s@UpdateThesaurus' {} a -> s {description = a} :: UpdateThesaurus)

-- | A new name for the thesaurus.
updateThesaurus_name :: Lens.Lens' UpdateThesaurus (Prelude.Maybe Prelude.Text)
updateThesaurus_name = Lens.lens (\UpdateThesaurus' {name} -> name) (\s@UpdateThesaurus' {} a -> s {name = a} :: UpdateThesaurus)

-- | An IAM role that gives Amazon Kendra permissions to access thesaurus
-- file specified in @SourceS3Path@.
updateThesaurus_roleArn :: Lens.Lens' UpdateThesaurus (Prelude.Maybe Prelude.Text)
updateThesaurus_roleArn = Lens.lens (\UpdateThesaurus' {roleArn} -> roleArn) (\s@UpdateThesaurus' {} a -> s {roleArn = a} :: UpdateThesaurus)

-- | Undocumented member.
updateThesaurus_sourceS3Path :: Lens.Lens' UpdateThesaurus (Prelude.Maybe S3Path)
updateThesaurus_sourceS3Path = Lens.lens (\UpdateThesaurus' {sourceS3Path} -> sourceS3Path) (\s@UpdateThesaurus' {} a -> s {sourceS3Path = a} :: UpdateThesaurus)

-- | The identifier of the thesaurus you want to update.
updateThesaurus_id :: Lens.Lens' UpdateThesaurus Prelude.Text
updateThesaurus_id = Lens.lens (\UpdateThesaurus' {id} -> id) (\s@UpdateThesaurus' {} a -> s {id = a} :: UpdateThesaurus)

-- | The identifier of the index for the thesaurus.
updateThesaurus_indexId :: Lens.Lens' UpdateThesaurus Prelude.Text
updateThesaurus_indexId = Lens.lens (\UpdateThesaurus' {indexId} -> indexId) (\s@UpdateThesaurus' {} a -> s {indexId = a} :: UpdateThesaurus)

instance Core.AWSRequest UpdateThesaurus where
  type
    AWSResponse UpdateThesaurus =
      UpdateThesaurusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateThesaurusResponse'

instance Prelude.Hashable UpdateThesaurus where
  hashWithSalt _salt UpdateThesaurus' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` sourceS3Path
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData UpdateThesaurus where
  rnf UpdateThesaurus' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf sourceS3Path
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf indexId

instance Data.ToHeaders UpdateThesaurus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.UpdateThesaurus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateThesaurus where
  toJSON UpdateThesaurus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("SourceS3Path" Data..=) Prelude.<$> sourceS3Path,
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("IndexId" Data..= indexId)
          ]
      )

instance Data.ToPath UpdateThesaurus where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateThesaurus where
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

instance Prelude.NFData UpdateThesaurusResponse where
  rnf _ = ()
