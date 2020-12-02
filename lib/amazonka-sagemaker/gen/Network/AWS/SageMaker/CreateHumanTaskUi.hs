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
-- Module      : Network.AWS.SageMaker.CreateHumanTaskUi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines the settings you will use for the human review workflow user interface. Reviewers will see a three-panel interface with an instruction area, the item to review, and an input area.
module Network.AWS.SageMaker.CreateHumanTaskUi
  ( -- * Creating a Request
    createHumanTaskUi,
    CreateHumanTaskUi,

    -- * Request Lenses
    chtuTags,
    chtuHumanTaskUiName,
    chtuUiTemplate,

    -- * Destructuring the Response
    createHumanTaskUiResponse,
    CreateHumanTaskUiResponse,

    -- * Response Lenses
    chtursResponseStatus,
    chtursHumanTaskUiARN,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'createHumanTaskUi' smart constructor.
data CreateHumanTaskUi = CreateHumanTaskUi'
  { _chtuTags ::
      !(Maybe [Tag]),
    _chtuHumanTaskUiName :: !Text,
    _chtuUiTemplate :: !UiTemplate
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateHumanTaskUi' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chtuTags' - An array of key-value pairs that contain metadata to help you categorize and organize a human review workflow user interface. Each tag consists of a key and a value, both of which you define.
--
-- * 'chtuHumanTaskUiName' - The name of the user interface you are creating.
--
-- * 'chtuUiTemplate' - Undocumented member.
createHumanTaskUi ::
  -- | 'chtuHumanTaskUiName'
  Text ->
  -- | 'chtuUiTemplate'
  UiTemplate ->
  CreateHumanTaskUi
createHumanTaskUi pHumanTaskUiName_ pUiTemplate_ =
  CreateHumanTaskUi'
    { _chtuTags = Nothing,
      _chtuHumanTaskUiName = pHumanTaskUiName_,
      _chtuUiTemplate = pUiTemplate_
    }

-- | An array of key-value pairs that contain metadata to help you categorize and organize a human review workflow user interface. Each tag consists of a key and a value, both of which you define.
chtuTags :: Lens' CreateHumanTaskUi [Tag]
chtuTags = lens _chtuTags (\s a -> s {_chtuTags = a}) . _Default . _Coerce

-- | The name of the user interface you are creating.
chtuHumanTaskUiName :: Lens' CreateHumanTaskUi Text
chtuHumanTaskUiName = lens _chtuHumanTaskUiName (\s a -> s {_chtuHumanTaskUiName = a})

-- | Undocumented member.
chtuUiTemplate :: Lens' CreateHumanTaskUi UiTemplate
chtuUiTemplate = lens _chtuUiTemplate (\s a -> s {_chtuUiTemplate = a})

instance AWSRequest CreateHumanTaskUi where
  type Rs CreateHumanTaskUi = CreateHumanTaskUiResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          CreateHumanTaskUiResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "HumanTaskUiArn")
      )

instance Hashable CreateHumanTaskUi

instance NFData CreateHumanTaskUi

instance ToHeaders CreateHumanTaskUi where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.CreateHumanTaskUi" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateHumanTaskUi where
  toJSON CreateHumanTaskUi' {..} =
    object
      ( catMaybes
          [ ("Tags" .=) <$> _chtuTags,
            Just ("HumanTaskUiName" .= _chtuHumanTaskUiName),
            Just ("UiTemplate" .= _chtuUiTemplate)
          ]
      )

instance ToPath CreateHumanTaskUi where
  toPath = const "/"

instance ToQuery CreateHumanTaskUi where
  toQuery = const mempty

-- | /See:/ 'createHumanTaskUiResponse' smart constructor.
data CreateHumanTaskUiResponse = CreateHumanTaskUiResponse'
  { _chtursResponseStatus ::
      !Int,
    _chtursHumanTaskUiARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateHumanTaskUiResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chtursResponseStatus' - -- | The response status code.
--
-- * 'chtursHumanTaskUiARN' - The Amazon Resource Name (ARN) of the human review workflow user interface you create.
createHumanTaskUiResponse ::
  -- | 'chtursResponseStatus'
  Int ->
  -- | 'chtursHumanTaskUiARN'
  Text ->
  CreateHumanTaskUiResponse
createHumanTaskUiResponse pResponseStatus_ pHumanTaskUiARN_ =
  CreateHumanTaskUiResponse'
    { _chtursResponseStatus =
        pResponseStatus_,
      _chtursHumanTaskUiARN = pHumanTaskUiARN_
    }

-- | -- | The response status code.
chtursResponseStatus :: Lens' CreateHumanTaskUiResponse Int
chtursResponseStatus = lens _chtursResponseStatus (\s a -> s {_chtursResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the human review workflow user interface you create.
chtursHumanTaskUiARN :: Lens' CreateHumanTaskUiResponse Text
chtursHumanTaskUiARN = lens _chtursHumanTaskUiARN (\s a -> s {_chtursHumanTaskUiARN = a})

instance NFData CreateHumanTaskUiResponse
