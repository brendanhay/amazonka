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
-- Module      : Amazonka.Transcribe.DeleteMedicalVocabulary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom medical vocabulary. To use this operation, specify the
-- name of the vocabulary you want to delete using @VocabularyName@.
-- Vocabulary names are case sensitive.
module Amazonka.Transcribe.DeleteMedicalVocabulary
  ( -- * Creating a Request
    DeleteMedicalVocabulary (..),
    newDeleteMedicalVocabulary,

    -- * Request Lenses
    deleteMedicalVocabulary_vocabularyName,

    -- * Destructuring the Response
    DeleteMedicalVocabularyResponse (..),
    newDeleteMedicalVocabularyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newDeleteMedicalVocabulary' smart constructor.
data DeleteMedicalVocabulary = DeleteMedicalVocabulary'
  { -- | The name of the custom medical vocabulary you want to delete. Vocabulary
    -- names are case sensitive.
    vocabularyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMedicalVocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyName', 'deleteMedicalVocabulary_vocabularyName' - The name of the custom medical vocabulary you want to delete. Vocabulary
-- names are case sensitive.
newDeleteMedicalVocabulary ::
  -- | 'vocabularyName'
  Prelude.Text ->
  DeleteMedicalVocabulary
newDeleteMedicalVocabulary pVocabularyName_ =
  DeleteMedicalVocabulary'
    { vocabularyName =
        pVocabularyName_
    }

-- | The name of the custom medical vocabulary you want to delete. Vocabulary
-- names are case sensitive.
deleteMedicalVocabulary_vocabularyName :: Lens.Lens' DeleteMedicalVocabulary Prelude.Text
deleteMedicalVocabulary_vocabularyName = Lens.lens (\DeleteMedicalVocabulary' {vocabularyName} -> vocabularyName) (\s@DeleteMedicalVocabulary' {} a -> s {vocabularyName = a} :: DeleteMedicalVocabulary)

instance Core.AWSRequest DeleteMedicalVocabulary where
  type
    AWSResponse DeleteMedicalVocabulary =
      DeleteMedicalVocabularyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteMedicalVocabularyResponse'

instance Prelude.Hashable DeleteMedicalVocabulary where
  hashWithSalt _salt DeleteMedicalVocabulary' {..} =
    _salt `Prelude.hashWithSalt` vocabularyName

instance Prelude.NFData DeleteMedicalVocabulary where
  rnf DeleteMedicalVocabulary' {..} =
    Prelude.rnf vocabularyName

instance Core.ToHeaders DeleteMedicalVocabulary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.DeleteMedicalVocabulary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteMedicalVocabulary where
  toJSON DeleteMedicalVocabulary' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("VocabularyName" Core..= vocabularyName)
          ]
      )

instance Core.ToPath DeleteMedicalVocabulary where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteMedicalVocabulary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMedicalVocabularyResponse' smart constructor.
data DeleteMedicalVocabularyResponse = DeleteMedicalVocabularyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMedicalVocabularyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteMedicalVocabularyResponse ::
  DeleteMedicalVocabularyResponse
newDeleteMedicalVocabularyResponse =
  DeleteMedicalVocabularyResponse'

instance
  Prelude.NFData
    DeleteMedicalVocabularyResponse
  where
  rnf _ = ()
