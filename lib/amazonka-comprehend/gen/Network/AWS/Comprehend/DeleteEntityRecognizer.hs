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
-- Module      : Network.AWS.Comprehend.DeleteEntityRecognizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an entity recognizer.
--
--
-- Only those recognizers that are in terminated states (IN_ERROR, TRAINED) will be deleted. If an active inference job is using the model, a @ResourceInUseException@ will be returned.
--
-- This is an asynchronous action that puts the recognizer into a DELETING state, and it is then removed by a background job. Once removed, the recognizer disappears from your account and is no longer available for use.
module Network.AWS.Comprehend.DeleteEntityRecognizer
  ( -- * Creating a Request
    deleteEntityRecognizer,
    DeleteEntityRecognizer,

    -- * Request Lenses
    derEntityRecognizerARN,

    -- * Destructuring the Response
    deleteEntityRecognizerResponse,
    DeleteEntityRecognizerResponse,

    -- * Response Lenses
    derrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteEntityRecognizer' smart constructor.
newtype DeleteEntityRecognizer = DeleteEntityRecognizer'
  { _derEntityRecognizerARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteEntityRecognizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'derEntityRecognizerARN' - The Amazon Resource Name (ARN) that identifies the entity recognizer.
deleteEntityRecognizer ::
  -- | 'derEntityRecognizerARN'
  Text ->
  DeleteEntityRecognizer
deleteEntityRecognizer pEntityRecognizerARN_ =
  DeleteEntityRecognizer'
    { _derEntityRecognizerARN =
        pEntityRecognizerARN_
    }

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
derEntityRecognizerARN :: Lens' DeleteEntityRecognizer Text
derEntityRecognizerARN = lens _derEntityRecognizerARN (\s a -> s {_derEntityRecognizerARN = a})

instance AWSRequest DeleteEntityRecognizer where
  type Rs DeleteEntityRecognizer = DeleteEntityRecognizerResponse
  request = postJSON comprehend
  response =
    receiveEmpty
      ( \s h x ->
          DeleteEntityRecognizerResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteEntityRecognizer

instance NFData DeleteEntityRecognizer

instance ToHeaders DeleteEntityRecognizer where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.DeleteEntityRecognizer" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteEntityRecognizer where
  toJSON DeleteEntityRecognizer' {..} =
    object
      ( catMaybes
          [Just ("EntityRecognizerArn" .= _derEntityRecognizerARN)]
      )

instance ToPath DeleteEntityRecognizer where
  toPath = const "/"

instance ToQuery DeleteEntityRecognizer where
  toQuery = const mempty

-- | /See:/ 'deleteEntityRecognizerResponse' smart constructor.
newtype DeleteEntityRecognizerResponse = DeleteEntityRecognizerResponse'
  { _derrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteEntityRecognizerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'derrsResponseStatus' - -- | The response status code.
deleteEntityRecognizerResponse ::
  -- | 'derrsResponseStatus'
  Int ->
  DeleteEntityRecognizerResponse
deleteEntityRecognizerResponse pResponseStatus_ =
  DeleteEntityRecognizerResponse'
    { _derrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
derrsResponseStatus :: Lens' DeleteEntityRecognizerResponse Int
derrsResponseStatus = lens _derrsResponseStatus (\s a -> s {_derrsResponseStatus = a})

instance NFData DeleteEntityRecognizerResponse
