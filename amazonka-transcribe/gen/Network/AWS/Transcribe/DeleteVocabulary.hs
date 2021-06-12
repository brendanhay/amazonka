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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newDeleteVocabulary' smart constructor.
data DeleteVocabulary = DeleteVocabulary'
  { -- | The name of the vocabulary to delete.
    vocabularyName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteVocabulary
newDeleteVocabulary pVocabularyName_ =
  DeleteVocabulary'
    { vocabularyName =
        pVocabularyName_
    }

-- | The name of the vocabulary to delete.
deleteVocabulary_vocabularyName :: Lens.Lens' DeleteVocabulary Core.Text
deleteVocabulary_vocabularyName = Lens.lens (\DeleteVocabulary' {vocabularyName} -> vocabularyName) (\s@DeleteVocabulary' {} a -> s {vocabularyName = a} :: DeleteVocabulary)

instance Core.AWSRequest DeleteVocabulary where
  type
    AWSResponse DeleteVocabulary =
      DeleteVocabularyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteVocabularyResponse'

instance Core.Hashable DeleteVocabulary

instance Core.NFData DeleteVocabulary

instance Core.ToHeaders DeleteVocabulary where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Transcribe.DeleteVocabulary" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteVocabulary where
  toJSON DeleteVocabulary' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("VocabularyName" Core..= vocabularyName)
          ]
      )

instance Core.ToPath DeleteVocabulary where
  toPath = Core.const "/"

instance Core.ToQuery DeleteVocabulary where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteVocabularyResponse' smart constructor.
data DeleteVocabularyResponse = DeleteVocabularyResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteVocabularyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVocabularyResponse ::
  DeleteVocabularyResponse
newDeleteVocabularyResponse =
  DeleteVocabularyResponse'

instance Core.NFData DeleteVocabularyResponse
