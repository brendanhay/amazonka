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
-- Module      : Network.AWS.Transcribe.DeleteMedicalVocabulary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a vocabulary from Amazon Transcribe Medical.
module Network.AWS.Transcribe.DeleteMedicalVocabulary
  ( -- * Creating a Request
    deleteMedicalVocabulary,
    DeleteMedicalVocabulary,

    -- * Request Lenses
    dmvVocabularyName,

    -- * Destructuring the Response
    deleteMedicalVocabularyResponse,
    DeleteMedicalVocabularyResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'deleteMedicalVocabulary' smart constructor.
newtype DeleteMedicalVocabulary = DeleteMedicalVocabulary'
  { _dmvVocabularyName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteMedicalVocabulary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmvVocabularyName' - The name of the vocabulary that you want to delete.
deleteMedicalVocabulary ::
  -- | 'dmvVocabularyName'
  Text ->
  DeleteMedicalVocabulary
deleteMedicalVocabulary pVocabularyName_ =
  DeleteMedicalVocabulary' {_dmvVocabularyName = pVocabularyName_}

-- | The name of the vocabulary that you want to delete.
dmvVocabularyName :: Lens' DeleteMedicalVocabulary Text
dmvVocabularyName = lens _dmvVocabularyName (\s a -> s {_dmvVocabularyName = a})

instance AWSRequest DeleteMedicalVocabulary where
  type Rs DeleteMedicalVocabulary = DeleteMedicalVocabularyResponse
  request = postJSON transcribe
  response = receiveNull DeleteMedicalVocabularyResponse'

instance Hashable DeleteMedicalVocabulary

instance NFData DeleteMedicalVocabulary

instance ToHeaders DeleteMedicalVocabulary where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Transcribe.DeleteMedicalVocabulary" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteMedicalVocabulary where
  toJSON DeleteMedicalVocabulary' {..} =
    object
      (catMaybes [Just ("VocabularyName" .= _dmvVocabularyName)])

instance ToPath DeleteMedicalVocabulary where
  toPath = const "/"

instance ToQuery DeleteMedicalVocabulary where
  toQuery = const mempty

-- | /See:/ 'deleteMedicalVocabularyResponse' smart constructor.
data DeleteMedicalVocabularyResponse = DeleteMedicalVocabularyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteMedicalVocabularyResponse' with the minimum fields required to make a request.
deleteMedicalVocabularyResponse ::
  DeleteMedicalVocabularyResponse
deleteMedicalVocabularyResponse = DeleteMedicalVocabularyResponse'

instance NFData DeleteMedicalVocabularyResponse
