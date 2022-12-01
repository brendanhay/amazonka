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
-- Module      : Amazonka.Transcribe.DeleteVocabulary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom vocabulary. To use this operation, specify the name of
-- the vocabulary you want to delete using @VocabularyName@. Vocabulary
-- names are case sensitive.
module Amazonka.Transcribe.DeleteVocabulary
  ( -- * Creating a Request
    DeleteVocabulary (..),
    newDeleteVocabulary,

    -- * Request Lenses
    deleteVocabulary_vocabularyName,

    -- * Destructuring the Response
    DeleteVocabularyResponse (..),
    newDeleteVocabularyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newDeleteVocabulary' smart constructor.
data DeleteVocabulary = DeleteVocabulary'
  { -- | The name of the custom vocabulary you want to delete. Vocabulary names
    -- are case sensitive.
    vocabularyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyName', 'deleteVocabulary_vocabularyName' - The name of the custom vocabulary you want to delete. Vocabulary names
-- are case sensitive.
newDeleteVocabulary ::
  -- | 'vocabularyName'
  Prelude.Text ->
  DeleteVocabulary
newDeleteVocabulary pVocabularyName_ =
  DeleteVocabulary'
    { vocabularyName =
        pVocabularyName_
    }

-- | The name of the custom vocabulary you want to delete. Vocabulary names
-- are case sensitive.
deleteVocabulary_vocabularyName :: Lens.Lens' DeleteVocabulary Prelude.Text
deleteVocabulary_vocabularyName = Lens.lens (\DeleteVocabulary' {vocabularyName} -> vocabularyName) (\s@DeleteVocabulary' {} a -> s {vocabularyName = a} :: DeleteVocabulary)

instance Core.AWSRequest DeleteVocabulary where
  type
    AWSResponse DeleteVocabulary =
      DeleteVocabularyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteVocabularyResponse'

instance Prelude.Hashable DeleteVocabulary where
  hashWithSalt _salt DeleteVocabulary' {..} =
    _salt `Prelude.hashWithSalt` vocabularyName

instance Prelude.NFData DeleteVocabulary where
  rnf DeleteVocabulary' {..} =
    Prelude.rnf vocabularyName

instance Core.ToHeaders DeleteVocabulary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.DeleteVocabulary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteVocabulary where
  toJSON DeleteVocabulary' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("VocabularyName" Core..= vocabularyName)
          ]
      )

instance Core.ToPath DeleteVocabulary where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteVocabulary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVocabularyResponse' smart constructor.
data DeleteVocabularyResponse = DeleteVocabularyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVocabularyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVocabularyResponse ::
  DeleteVocabularyResponse
newDeleteVocabularyResponse =
  DeleteVocabularyResponse'

instance Prelude.NFData DeleteVocabularyResponse where
  rnf _ = ()
