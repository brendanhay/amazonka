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
-- Module      : Network.AWS.Comprehend.DetectEntities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects text for named entities, and returns information about them. For more information, about named entities, see 'how-entities' .
module Network.AWS.Comprehend.DetectEntities
  ( -- * Creating a Request
    detectEntities,
    DetectEntities,

    -- * Request Lenses
    dLanguageCode,
    dEndpointARN,
    dText,

    -- * Destructuring the Response
    detectEntitiesResponse,
    DetectEntitiesResponse,

    -- * Response Lenses
    deersEntities,
    deersResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detectEntities' smart constructor.
data DetectEntities = DetectEntities'
  { _dLanguageCode ::
      !(Maybe LanguageCode),
    _dEndpointARN :: !(Maybe Text),
    _dText :: !(Sensitive Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectEntities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dLanguageCode' - The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language. If your request includes the endpoint for a custom entity recognition model, Amazon Comprehend uses the language of your custom model, and it ignores any language code that you specify here.
--
-- * 'dEndpointARN' - The Amazon Resource Name of an endpoint that is associated with a custom entity recognition model. Provide an endpoint if you want to detect entities by using your own custom model instead of the default model that is used by Amazon Comprehend. If you specify an endpoint, Amazon Comprehend uses the language of your custom model, and it ignores any language code that you provide in your request.
--
-- * 'dText' - A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
detectEntities ::
  -- | 'dText'
  Text ->
  DetectEntities
detectEntities pText_ =
  DetectEntities'
    { _dLanguageCode = Nothing,
      _dEndpointARN = Nothing,
      _dText = _Sensitive # pText_
    }

-- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language. If your request includes the endpoint for a custom entity recognition model, Amazon Comprehend uses the language of your custom model, and it ignores any language code that you specify here.
dLanguageCode :: Lens' DetectEntities (Maybe LanguageCode)
dLanguageCode = lens _dLanguageCode (\s a -> s {_dLanguageCode = a})

-- | The Amazon Resource Name of an endpoint that is associated with a custom entity recognition model. Provide an endpoint if you want to detect entities by using your own custom model instead of the default model that is used by Amazon Comprehend. If you specify an endpoint, Amazon Comprehend uses the language of your custom model, and it ignores any language code that you provide in your request.
dEndpointARN :: Lens' DetectEntities (Maybe Text)
dEndpointARN = lens _dEndpointARN (\s a -> s {_dEndpointARN = a})

-- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
dText :: Lens' DetectEntities Text
dText = lens _dText (\s a -> s {_dText = a}) . _Sensitive

instance AWSRequest DetectEntities where
  type Rs DetectEntities = DetectEntitiesResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          DetectEntitiesResponse'
            <$> (x .?> "Entities" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable DetectEntities

instance NFData DetectEntities

instance ToHeaders DetectEntities where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.DetectEntities" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DetectEntities where
  toJSON DetectEntities' {..} =
    object
      ( catMaybes
          [ ("LanguageCode" .=) <$> _dLanguageCode,
            ("EndpointArn" .=) <$> _dEndpointARN,
            Just ("Text" .= _dText)
          ]
      )

instance ToPath DetectEntities where
  toPath = const "/"

instance ToQuery DetectEntities where
  toQuery = const mempty

-- | /See:/ 'detectEntitiesResponse' smart constructor.
data DetectEntitiesResponse = DetectEntitiesResponse'
  { _deersEntities ::
      !(Maybe [Entity]),
    _deersResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectEntitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deersEntities' - A collection of entities identified in the input text. For each entity, the response provides the entity text, entity type, where the entity text begins and ends, and the level of confidence that Amazon Comprehend has in the detection.  If your request uses a custom entity recognition model, Amazon Comprehend detects the entities that the model is trained to recognize. Otherwise, it detects the default entity types. For a list of default entity types, see 'how-entities' .
--
-- * 'deersResponseStatus' - -- | The response status code.
detectEntitiesResponse ::
  -- | 'deersResponseStatus'
  Int ->
  DetectEntitiesResponse
detectEntitiesResponse pResponseStatus_ =
  DetectEntitiesResponse'
    { _deersEntities = Nothing,
      _deersResponseStatus = pResponseStatus_
    }

-- | A collection of entities identified in the input text. For each entity, the response provides the entity text, entity type, where the entity text begins and ends, and the level of confidence that Amazon Comprehend has in the detection.  If your request uses a custom entity recognition model, Amazon Comprehend detects the entities that the model is trained to recognize. Otherwise, it detects the default entity types. For a list of default entity types, see 'how-entities' .
deersEntities :: Lens' DetectEntitiesResponse [Entity]
deersEntities = lens _deersEntities (\s a -> s {_deersEntities = a}) . _Default . _Coerce

-- | -- | The response status code.
deersResponseStatus :: Lens' DetectEntitiesResponse Int
deersResponseStatus = lens _deersResponseStatus (\s a -> s {_deersResponseStatus = a})

instance NFData DetectEntitiesResponse
