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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newDeleteMedicalVocabulary' smart constructor.
data DeleteMedicalVocabulary = DeleteMedicalVocabulary'
  { -- | The name of the vocabulary that you want to delete.
    vocabularyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteMedicalVocabulary
newDeleteMedicalVocabulary pVocabularyName_ =
  DeleteMedicalVocabulary'
    { vocabularyName =
        pVocabularyName_
    }

-- | The name of the vocabulary that you want to delete.
deleteMedicalVocabulary_vocabularyName :: Lens.Lens' DeleteMedicalVocabulary Prelude.Text
deleteMedicalVocabulary_vocabularyName = Lens.lens (\DeleteMedicalVocabulary' {vocabularyName} -> vocabularyName) (\s@DeleteMedicalVocabulary' {} a -> s {vocabularyName = a} :: DeleteMedicalVocabulary)

instance Prelude.AWSRequest DeleteMedicalVocabulary where
  type
    Rs DeleteMedicalVocabulary =
      DeleteMedicalVocabularyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteMedicalVocabularyResponse'

instance Prelude.Hashable DeleteMedicalVocabulary

instance Prelude.NFData DeleteMedicalVocabulary

instance Prelude.ToHeaders DeleteMedicalVocabulary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Transcribe.DeleteMedicalVocabulary" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteMedicalVocabulary where
  toJSON DeleteMedicalVocabulary' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("VocabularyName" Prelude..= vocabularyName)
          ]
      )

instance Prelude.ToPath DeleteMedicalVocabulary where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteMedicalVocabulary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMedicalVocabularyResponse' smart constructor.
data DeleteMedicalVocabularyResponse = DeleteMedicalVocabularyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
