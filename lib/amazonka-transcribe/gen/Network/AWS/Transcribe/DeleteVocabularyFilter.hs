{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.DeleteVocabularyFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a vocabulary filter.
module Network.AWS.Transcribe.DeleteVocabularyFilter
  ( -- * Creating a Request
    deleteVocabularyFilter,
    DeleteVocabularyFilter,

    -- * Request Lenses
    dvfVocabularyFilterName,

    -- * Destructuring the Response
    deleteVocabularyFilterResponse,
    DeleteVocabularyFilterResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'deleteVocabularyFilter' smart constructor.
newtype DeleteVocabularyFilter = DeleteVocabularyFilter'
  { _dvfVocabularyFilterName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteVocabularyFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvfVocabularyFilterName' - The name of the vocabulary filter to remove.
deleteVocabularyFilter ::
  -- | 'dvfVocabularyFilterName'
  Text ->
  DeleteVocabularyFilter
deleteVocabularyFilter pVocabularyFilterName_ =
  DeleteVocabularyFilter'
    { _dvfVocabularyFilterName =
        pVocabularyFilterName_
    }

-- | The name of the vocabulary filter to remove.
dvfVocabularyFilterName :: Lens' DeleteVocabularyFilter Text
dvfVocabularyFilterName = lens _dvfVocabularyFilterName (\s a -> s {_dvfVocabularyFilterName = a})

instance AWSRequest DeleteVocabularyFilter where
  type Rs DeleteVocabularyFilter = DeleteVocabularyFilterResponse
  request = postJSON transcribe
  response = receiveNull DeleteVocabularyFilterResponse'

instance Hashable DeleteVocabularyFilter

instance NFData DeleteVocabularyFilter

instance ToHeaders DeleteVocabularyFilter where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Transcribe.DeleteVocabularyFilter" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteVocabularyFilter where
  toJSON DeleteVocabularyFilter' {..} =
    object
      ( catMaybes
          [Just ("VocabularyFilterName" .= _dvfVocabularyFilterName)]
      )

instance ToPath DeleteVocabularyFilter where
  toPath = const "/"

instance ToQuery DeleteVocabularyFilter where
  toQuery = const mempty

-- | /See:/ 'deleteVocabularyFilterResponse' smart constructor.
data DeleteVocabularyFilterResponse = DeleteVocabularyFilterResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteVocabularyFilterResponse' with the minimum fields required to make a request.
deleteVocabularyFilterResponse ::
  DeleteVocabularyFilterResponse
deleteVocabularyFilterResponse = DeleteVocabularyFilterResponse'

instance NFData DeleteVocabularyFilterResponse
