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
-- Module      : Network.AWS.Transcribe.DeleteVocabularyFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a vocabulary filter.
module Network.AWS.Transcribe.DeleteVocabularyFilter
  ( -- * Creating a Request
    DeleteVocabularyFilter (..),
    newDeleteVocabularyFilter,

    -- * Request Lenses
    deleteVocabularyFilter_vocabularyFilterName,

    -- * Destructuring the Response
    DeleteVocabularyFilterResponse (..),
    newDeleteVocabularyFilterResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newDeleteVocabularyFilter' smart constructor.
data DeleteVocabularyFilter = DeleteVocabularyFilter'
  { -- | The name of the vocabulary filter to remove.
    vocabularyFilterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVocabularyFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyFilterName', 'deleteVocabularyFilter_vocabularyFilterName' - The name of the vocabulary filter to remove.
newDeleteVocabularyFilter ::
  -- | 'vocabularyFilterName'
  Prelude.Text ->
  DeleteVocabularyFilter
newDeleteVocabularyFilter pVocabularyFilterName_ =
  DeleteVocabularyFilter'
    { vocabularyFilterName =
        pVocabularyFilterName_
    }

-- | The name of the vocabulary filter to remove.
deleteVocabularyFilter_vocabularyFilterName :: Lens.Lens' DeleteVocabularyFilter Prelude.Text
deleteVocabularyFilter_vocabularyFilterName = Lens.lens (\DeleteVocabularyFilter' {vocabularyFilterName} -> vocabularyFilterName) (\s@DeleteVocabularyFilter' {} a -> s {vocabularyFilterName = a} :: DeleteVocabularyFilter)

instance Prelude.AWSRequest DeleteVocabularyFilter where
  type
    Rs DeleteVocabularyFilter =
      DeleteVocabularyFilterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteVocabularyFilterResponse'

instance Prelude.Hashable DeleteVocabularyFilter

instance Prelude.NFData DeleteVocabularyFilter

instance Prelude.ToHeaders DeleteVocabularyFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Transcribe.DeleteVocabularyFilter" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteVocabularyFilter where
  toJSON DeleteVocabularyFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "VocabularyFilterName"
                  Prelude..= vocabularyFilterName
              )
          ]
      )

instance Prelude.ToPath DeleteVocabularyFilter where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteVocabularyFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVocabularyFilterResponse' smart constructor.
data DeleteVocabularyFilterResponse = DeleteVocabularyFilterResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVocabularyFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVocabularyFilterResponse ::
  DeleteVocabularyFilterResponse
newDeleteVocabularyFilterResponse =
  DeleteVocabularyFilterResponse'

instance
  Prelude.NFData
    DeleteVocabularyFilterResponse
