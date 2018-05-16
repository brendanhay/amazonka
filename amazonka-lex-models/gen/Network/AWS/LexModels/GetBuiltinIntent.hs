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
-- Module      : Network.AWS.LexModels.GetBuiltinIntent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a built-in intent.
--
--
-- This operation requires permission for the @lex:GetBuiltinIntent@ action.
--
module Network.AWS.LexModels.GetBuiltinIntent
    (
    -- * Creating a Request
      getBuiltinIntent
    , GetBuiltinIntent
    -- * Request Lenses
    , gbiSignature

    -- * Destructuring the Response
    , getBuiltinIntentResponse
    , GetBuiltinIntentResponse
    -- * Response Lenses
    , gbirsSignature
    , gbirsSlots
    , gbirsSupportedLocales
    , gbirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBuiltinIntent' smart constructor.
newtype GetBuiltinIntent = GetBuiltinIntent'
  { _gbiSignature :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBuiltinIntent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbiSignature' - The unique identifier for a built-in intent. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
getBuiltinIntent
    :: Text -- ^ 'gbiSignature'
    -> GetBuiltinIntent
getBuiltinIntent pSignature_ = GetBuiltinIntent' {_gbiSignature = pSignature_}


-- | The unique identifier for a built-in intent. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
gbiSignature :: Lens' GetBuiltinIntent Text
gbiSignature = lens _gbiSignature (\ s a -> s{_gbiSignature = a})

instance AWSRequest GetBuiltinIntent where
        type Rs GetBuiltinIntent = GetBuiltinIntentResponse
        request = get lexModels
        response
          = receiveJSON
              (\ s h x ->
                 GetBuiltinIntentResponse' <$>
                   (x .?> "signature") <*> (x .?> "slots" .!@ mempty)
                     <*> (x .?> "supportedLocales" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetBuiltinIntent where

instance NFData GetBuiltinIntent where

instance ToHeaders GetBuiltinIntent where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetBuiltinIntent where
        toPath GetBuiltinIntent'{..}
          = mconcat ["/builtins/intents/", toBS _gbiSignature]

instance ToQuery GetBuiltinIntent where
        toQuery = const mempty

-- | /See:/ 'getBuiltinIntentResponse' smart constructor.
data GetBuiltinIntentResponse = GetBuiltinIntentResponse'
  { _gbirsSignature        :: !(Maybe Text)
  , _gbirsSlots            :: !(Maybe [BuiltinIntentSlot])
  , _gbirsSupportedLocales :: !(Maybe [Locale])
  , _gbirsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBuiltinIntentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbirsSignature' - The unique identifier for a built-in intent.
--
-- * 'gbirsSlots' - An array of @BuiltinIntentSlot@ objects, one entry for each slot type in the intent.
--
-- * 'gbirsSupportedLocales' - A list of locales that the intent supports.
--
-- * 'gbirsResponseStatus' - -- | The response status code.
getBuiltinIntentResponse
    :: Int -- ^ 'gbirsResponseStatus'
    -> GetBuiltinIntentResponse
getBuiltinIntentResponse pResponseStatus_ =
  GetBuiltinIntentResponse'
    { _gbirsSignature = Nothing
    , _gbirsSlots = Nothing
    , _gbirsSupportedLocales = Nothing
    , _gbirsResponseStatus = pResponseStatus_
    }


-- | The unique identifier for a built-in intent.
gbirsSignature :: Lens' GetBuiltinIntentResponse (Maybe Text)
gbirsSignature = lens _gbirsSignature (\ s a -> s{_gbirsSignature = a})

-- | An array of @BuiltinIntentSlot@ objects, one entry for each slot type in the intent.
gbirsSlots :: Lens' GetBuiltinIntentResponse [BuiltinIntentSlot]
gbirsSlots = lens _gbirsSlots (\ s a -> s{_gbirsSlots = a}) . _Default . _Coerce

-- | A list of locales that the intent supports.
gbirsSupportedLocales :: Lens' GetBuiltinIntentResponse [Locale]
gbirsSupportedLocales = lens _gbirsSupportedLocales (\ s a -> s{_gbirsSupportedLocales = a}) . _Default . _Coerce

-- | -- | The response status code.
gbirsResponseStatus :: Lens' GetBuiltinIntentResponse Int
gbirsResponseStatus = lens _gbirsResponseStatus (\ s a -> s{_gbirsResponseStatus = a})

instance NFData GetBuiltinIntentResponse where
