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
-- Module      : Network.AWS.Comprehend.DetectPiiEntities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects the input text for entities that contain personally identifiable information (PII) and returns information about them.
module Network.AWS.Comprehend.DetectPiiEntities
  ( -- * Creating a Request
    detectPiiEntities,
    DetectPiiEntities,

    -- * Request Lenses
    dpeText,
    dpeLanguageCode,

    -- * Destructuring the Response
    detectPiiEntitiesResponse,
    DetectPiiEntitiesResponse,

    -- * Response Lenses
    dpersEntities,
    dpersResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detectPiiEntities' smart constructor.
data DetectPiiEntities = DetectPiiEntities'
  { _dpeText :: !Text,
    _dpeLanguageCode :: !LanguageCode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectPiiEntities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpeText' - A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- * 'dpeLanguageCode' - The language of the input documents.
detectPiiEntities ::
  -- | 'dpeText'
  Text ->
  -- | 'dpeLanguageCode'
  LanguageCode ->
  DetectPiiEntities
detectPiiEntities pText_ pLanguageCode_ =
  DetectPiiEntities'
    { _dpeText = pText_,
      _dpeLanguageCode = pLanguageCode_
    }

-- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
dpeText :: Lens' DetectPiiEntities Text
dpeText = lens _dpeText (\s a -> s {_dpeText = a})

-- | The language of the input documents.
dpeLanguageCode :: Lens' DetectPiiEntities LanguageCode
dpeLanguageCode = lens _dpeLanguageCode (\s a -> s {_dpeLanguageCode = a})

instance AWSRequest DetectPiiEntities where
  type Rs DetectPiiEntities = DetectPiiEntitiesResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          DetectPiiEntitiesResponse'
            <$> (x .?> "Entities" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable DetectPiiEntities

instance NFData DetectPiiEntities

instance ToHeaders DetectPiiEntities where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.DetectPiiEntities" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DetectPiiEntities where
  toJSON DetectPiiEntities' {..} =
    object
      ( catMaybes
          [ Just ("Text" .= _dpeText),
            Just ("LanguageCode" .= _dpeLanguageCode)
          ]
      )

instance ToPath DetectPiiEntities where
  toPath = const "/"

instance ToQuery DetectPiiEntities where
  toQuery = const mempty

-- | /See:/ 'detectPiiEntitiesResponse' smart constructor.
data DetectPiiEntitiesResponse = DetectPiiEntitiesResponse'
  { _dpersEntities ::
      !(Maybe [PiiEntity]),
    _dpersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectPiiEntitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpersEntities' - A collection of PII entities identified in the input text. For each entity, the response provides the entity type, where the entity text begins and ends, and the level of confidence that Amazon Comprehend has in the detection.
--
-- * 'dpersResponseStatus' - -- | The response status code.
detectPiiEntitiesResponse ::
  -- | 'dpersResponseStatus'
  Int ->
  DetectPiiEntitiesResponse
detectPiiEntitiesResponse pResponseStatus_ =
  DetectPiiEntitiesResponse'
    { _dpersEntities = Nothing,
      _dpersResponseStatus = pResponseStatus_
    }

-- | A collection of PII entities identified in the input text. For each entity, the response provides the entity type, where the entity text begins and ends, and the level of confidence that Amazon Comprehend has in the detection.
dpersEntities :: Lens' DetectPiiEntitiesResponse [PiiEntity]
dpersEntities = lens _dpersEntities (\s a -> s {_dpersEntities = a}) . _Default . _Coerce

-- | -- | The response status code.
dpersResponseStatus :: Lens' DetectPiiEntitiesResponse Int
dpersResponseStatus = lens _dpersResponseStatus (\s a -> s {_dpersResponseStatus = a})

instance NFData DetectPiiEntitiesResponse
