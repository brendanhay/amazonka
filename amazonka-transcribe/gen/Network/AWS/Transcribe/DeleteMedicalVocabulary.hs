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
-- Module      : Network.AWS.Transcribe.DeleteMedicalVocabulary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a vocabulary from Amazon Transcribe Medical.
module Network.AWS.Transcribe.DeleteMedicalVocabulary
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newDeleteMedicalVocabulary' smart constructor.
data DeleteMedicalVocabulary = DeleteMedicalVocabulary'
  { -- | The name of the vocabulary that you want to delete.
    vocabularyName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteMedicalVocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyName', 'deleteMedicalVocabulary_vocabularyName' - The name of the vocabulary that you want to delete.
newDeleteMedicalVocabulary ::
  -- | 'vocabularyName'
  Core.Text ->
  DeleteMedicalVocabulary
newDeleteMedicalVocabulary pVocabularyName_ =
  DeleteMedicalVocabulary'
    { vocabularyName =
        pVocabularyName_
    }

-- | The name of the vocabulary that you want to delete.
deleteMedicalVocabulary_vocabularyName :: Lens.Lens' DeleteMedicalVocabulary Core.Text
deleteMedicalVocabulary_vocabularyName = Lens.lens (\DeleteMedicalVocabulary' {vocabularyName} -> vocabularyName) (\s@DeleteMedicalVocabulary' {} a -> s {vocabularyName = a} :: DeleteMedicalVocabulary)

instance Core.AWSRequest DeleteMedicalVocabulary where
  type
    AWSResponse DeleteMedicalVocabulary =
      DeleteMedicalVocabularyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteMedicalVocabularyResponse'

instance Core.Hashable DeleteMedicalVocabulary

instance Core.NFData DeleteMedicalVocabulary

instance Core.ToHeaders DeleteMedicalVocabulary where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.DeleteMedicalVocabulary" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteMedicalVocabulary where
  toJSON DeleteMedicalVocabulary' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("VocabularyName" Core..= vocabularyName)
          ]
      )

instance Core.ToPath DeleteMedicalVocabulary where
  toPath = Core.const "/"

instance Core.ToQuery DeleteMedicalVocabulary where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteMedicalVocabularyResponse' smart constructor.
data DeleteMedicalVocabularyResponse = DeleteMedicalVocabularyResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteMedicalVocabularyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteMedicalVocabularyResponse ::
  DeleteMedicalVocabularyResponse
newDeleteMedicalVocabularyResponse =
  DeleteMedicalVocabularyResponse'

instance Core.NFData DeleteMedicalVocabularyResponse
