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
-- Module      : Network.AWS.Transcribe.DeleteLanguageModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom language model using its name.
module Network.AWS.Transcribe.DeleteLanguageModel
  ( -- * Creating a Request
    deleteLanguageModel,
    DeleteLanguageModel,

    -- * Request Lenses
    dlmModelName,

    -- * Destructuring the Response
    deleteLanguageModelResponse,
    DeleteLanguageModelResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'deleteLanguageModel' smart constructor.
newtype DeleteLanguageModel = DeleteLanguageModel'
  { _dlmModelName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteLanguageModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlmModelName' - The name of the model you're choosing to delete.
deleteLanguageModel ::
  -- | 'dlmModelName'
  Text ->
  DeleteLanguageModel
deleteLanguageModel pModelName_ =
  DeleteLanguageModel' {_dlmModelName = pModelName_}

-- | The name of the model you're choosing to delete.
dlmModelName :: Lens' DeleteLanguageModel Text
dlmModelName = lens _dlmModelName (\s a -> s {_dlmModelName = a})

instance AWSRequest DeleteLanguageModel where
  type Rs DeleteLanguageModel = DeleteLanguageModelResponse
  request = postJSON transcribe
  response = receiveNull DeleteLanguageModelResponse'

instance Hashable DeleteLanguageModel

instance NFData DeleteLanguageModel

instance ToHeaders DeleteLanguageModel where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Transcribe.DeleteLanguageModel" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteLanguageModel where
  toJSON DeleteLanguageModel' {..} =
    object (catMaybes [Just ("ModelName" .= _dlmModelName)])

instance ToPath DeleteLanguageModel where
  toPath = const "/"

instance ToQuery DeleteLanguageModel where
  toQuery = const mempty

-- | /See:/ 'deleteLanguageModelResponse' smart constructor.
data DeleteLanguageModelResponse = DeleteLanguageModelResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteLanguageModelResponse' with the minimum fields required to make a request.
deleteLanguageModelResponse ::
  DeleteLanguageModelResponse
deleteLanguageModelResponse = DeleteLanguageModelResponse'

instance NFData DeleteLanguageModelResponse
