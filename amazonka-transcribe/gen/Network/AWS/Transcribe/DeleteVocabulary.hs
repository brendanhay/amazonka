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
-- Module      : Network.AWS.Transcribe.DeleteVocabulary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a vocabulary from Amazon Transcribe.
module Network.AWS.Transcribe.DeleteVocabulary
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newDeleteVocabulary' smart constructor.
data DeleteVocabulary = DeleteVocabulary'
  { -- | The name of the vocabulary to delete.
    vocabularyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyName', 'deleteVocabulary_vocabularyName' - The name of the vocabulary to delete.
newDeleteVocabulary ::
  -- | 'vocabularyName'
  Prelude.Text ->
  DeleteVocabulary
newDeleteVocabulary pVocabularyName_ =
  DeleteVocabulary'
    { vocabularyName =
        pVocabularyName_
    }

-- | The name of the vocabulary to delete.
deleteVocabulary_vocabularyName :: Lens.Lens' DeleteVocabulary Prelude.Text
deleteVocabulary_vocabularyName = Lens.lens (\DeleteVocabulary' {vocabularyName} -> vocabularyName) (\s@DeleteVocabulary' {} a -> s {vocabularyName = a} :: DeleteVocabulary)

instance Prelude.AWSRequest DeleteVocabulary where
  type Rs DeleteVocabulary = DeleteVocabularyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteVocabularyResponse'

instance Prelude.Hashable DeleteVocabulary

instance Prelude.NFData DeleteVocabulary

instance Prelude.ToHeaders DeleteVocabulary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Transcribe.DeleteVocabulary" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteVocabulary where
  toJSON DeleteVocabulary' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("VocabularyName" Prelude..= vocabularyName)
          ]
      )

instance Prelude.ToPath DeleteVocabulary where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteVocabulary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVocabularyResponse' smart constructor.
data DeleteVocabularyResponse = DeleteVocabularyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVocabularyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVocabularyResponse ::
  DeleteVocabularyResponse
newDeleteVocabularyResponse =
  DeleteVocabularyResponse'

instance Prelude.NFData DeleteVocabularyResponse
