{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DetectEntities
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects text for entities, and returns information about them. For more information, about entities, see 'how-entities' .
--
--
module Network.AWS.Comprehend.DetectEntities
    (
    -- * Creating a Request
      detectEntities
    , DetectEntities
    -- * Request Lenses
    , deText
    , deLanguageCode

    -- * Destructuring the Response
    , detectEntitiesResponse
    , DetectEntitiesResponse
    -- * Response Lenses
    , dersEntities
    , dersResponseStatus
    ) where

import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detectEntities' smart constructor.
data DetectEntities = DetectEntities'
  { _deText         :: !Text
  , _deLanguageCode :: !LanguageCode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectEntities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deText' - A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- * 'deLanguageCode' - The RFC 5646 language code of the input text. If the request does not specify the language code, the service detects the dominant language. If you specify a language code that the service does not support, it returns @UnsupportedLanguageException@ exception. For more information about RFC 5646, see <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on the /IETF Tools/ web site.
detectEntities
    :: Text -- ^ 'deText'
    -> LanguageCode -- ^ 'deLanguageCode'
    -> DetectEntities
detectEntities pText_ pLanguageCode_ =
  DetectEntities' {_deText = pText_, _deLanguageCode = pLanguageCode_}


-- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
deText :: Lens' DetectEntities Text
deText = lens _deText (\ s a -> s{_deText = a})

-- | The RFC 5646 language code of the input text. If the request does not specify the language code, the service detects the dominant language. If you specify a language code that the service does not support, it returns @UnsupportedLanguageException@ exception. For more information about RFC 5646, see <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on the /IETF Tools/ web site.
deLanguageCode :: Lens' DetectEntities LanguageCode
deLanguageCode = lens _deLanguageCode (\ s a -> s{_deLanguageCode = a})

instance AWSRequest DetectEntities where
        type Rs DetectEntities = DetectEntitiesResponse
        request = postJSON comprehend
        response
          = receiveJSON
              (\ s h x ->
                 DetectEntitiesResponse' <$>
                   (x .?> "Entities" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DetectEntities where

instance NFData DetectEntities where

instance ToHeaders DetectEntities where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Comprehend_20171127.DetectEntities" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DetectEntities where
        toJSON DetectEntities'{..}
          = object
              (catMaybes
                 [Just ("Text" .= _deText),
                  Just ("LanguageCode" .= _deLanguageCode)])

instance ToPath DetectEntities where
        toPath = const "/"

instance ToQuery DetectEntities where
        toQuery = const mempty

-- | /See:/ 'detectEntitiesResponse' smart constructor.
data DetectEntitiesResponse = DetectEntitiesResponse'
  { _dersEntities       :: !(Maybe [Entity])
  , _dersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectEntitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dersEntities' - A collection of entities identified in the input text. For each entity, the response provides the entity text, entity type, where the entity text begins and ends, and the level of confidence that Amazon Comprehend has in the detection. For a list of entity types, see 'how-entities' .
--
-- * 'dersResponseStatus' - -- | The response status code.
detectEntitiesResponse
    :: Int -- ^ 'dersResponseStatus'
    -> DetectEntitiesResponse
detectEntitiesResponse pResponseStatus_ =
  DetectEntitiesResponse'
    {_dersEntities = Nothing, _dersResponseStatus = pResponseStatus_}


-- | A collection of entities identified in the input text. For each entity, the response provides the entity text, entity type, where the entity text begins and ends, and the level of confidence that Amazon Comprehend has in the detection. For a list of entity types, see 'how-entities' .
dersEntities :: Lens' DetectEntitiesResponse [Entity]
dersEntities = lens _dersEntities (\ s a -> s{_dersEntities = a}) . _Default . _Coerce

-- | -- | The response status code.
dersResponseStatus :: Lens' DetectEntitiesResponse Int
dersResponseStatus = lens _dersResponseStatus (\ s a -> s{_dersResponseStatus = a})

instance NFData DetectEntitiesResponse where
