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
-- Module      : Network.AWS.Glue.UpdateClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing classifier (a @GrokClassifier@ , an @XMLClassifier@ , a @JsonClassifier@ , or a @CsvClassifier@ , depending on which field is present).
module Network.AWS.Glue.UpdateClassifier
  ( -- * Creating a Request
    updateClassifier,
    UpdateClassifier,

    -- * Request Lenses
    ucGrokClassifier,
    ucXMLClassifier,
    ucCSVClassifier,
    ucJSONClassifier,

    -- * Destructuring the Response
    updateClassifierResponse,
    UpdateClassifierResponse,

    -- * Response Lenses
    ursResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateClassifier' smart constructor.
data UpdateClassifier = UpdateClassifier'
  { _ucGrokClassifier ::
      !(Maybe UpdateGrokClassifierRequest),
    _ucXMLClassifier :: !(Maybe UpdateXMLClassifierRequest),
    _ucCSVClassifier :: !(Maybe UpdateCSVClassifierRequest),
    _ucJSONClassifier :: !(Maybe UpdateJSONClassifierRequest)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateClassifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucGrokClassifier' - A @GrokClassifier@ object with updated fields.
--
-- * 'ucXMLClassifier' - An @XMLClassifier@ object with updated fields.
--
-- * 'ucCSVClassifier' - A @CsvClassifier@ object with updated fields.
--
-- * 'ucJSONClassifier' - A @JsonClassifier@ object with updated fields.
updateClassifier ::
  UpdateClassifier
updateClassifier =
  UpdateClassifier'
    { _ucGrokClassifier = Nothing,
      _ucXMLClassifier = Nothing,
      _ucCSVClassifier = Nothing,
      _ucJSONClassifier = Nothing
    }

-- | A @GrokClassifier@ object with updated fields.
ucGrokClassifier :: Lens' UpdateClassifier (Maybe UpdateGrokClassifierRequest)
ucGrokClassifier = lens _ucGrokClassifier (\s a -> s {_ucGrokClassifier = a})

-- | An @XMLClassifier@ object with updated fields.
ucXMLClassifier :: Lens' UpdateClassifier (Maybe UpdateXMLClassifierRequest)
ucXMLClassifier = lens _ucXMLClassifier (\s a -> s {_ucXMLClassifier = a})

-- | A @CsvClassifier@ object with updated fields.
ucCSVClassifier :: Lens' UpdateClassifier (Maybe UpdateCSVClassifierRequest)
ucCSVClassifier = lens _ucCSVClassifier (\s a -> s {_ucCSVClassifier = a})

-- | A @JsonClassifier@ object with updated fields.
ucJSONClassifier :: Lens' UpdateClassifier (Maybe UpdateJSONClassifierRequest)
ucJSONClassifier = lens _ucJSONClassifier (\s a -> s {_ucJSONClassifier = a})

instance AWSRequest UpdateClassifier where
  type Rs UpdateClassifier = UpdateClassifierResponse
  request = postJSON glue
  response =
    receiveEmpty
      (\s h x -> UpdateClassifierResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateClassifier

instance NFData UpdateClassifier

instance ToHeaders UpdateClassifier where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.UpdateClassifier" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateClassifier where
  toJSON UpdateClassifier' {..} =
    object
      ( catMaybes
          [ ("GrokClassifier" .=) <$> _ucGrokClassifier,
            ("XMLClassifier" .=) <$> _ucXMLClassifier,
            ("CsvClassifier" .=) <$> _ucCSVClassifier,
            ("JsonClassifier" .=) <$> _ucJSONClassifier
          ]
      )

instance ToPath UpdateClassifier where
  toPath = const "/"

instance ToQuery UpdateClassifier where
  toQuery = const mempty

-- | /See:/ 'updateClassifierResponse' smart constructor.
newtype UpdateClassifierResponse = UpdateClassifierResponse'
  { _ursResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateClassifierResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ursResponseStatus' - -- | The response status code.
updateClassifierResponse ::
  -- | 'ursResponseStatus'
  Int ->
  UpdateClassifierResponse
updateClassifierResponse pResponseStatus_ =
  UpdateClassifierResponse' {_ursResponseStatus = pResponseStatus_}

-- | -- | The response status code.
ursResponseStatus :: Lens' UpdateClassifierResponse Int
ursResponseStatus = lens _ursResponseStatus (\s a -> s {_ursResponseStatus = a})

instance NFData UpdateClassifierResponse
