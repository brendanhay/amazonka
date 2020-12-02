{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.BuiltinIntentMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.BuiltinIntentMetadata where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.Locale
import Network.AWS.Prelude

-- | Provides metadata for a built-in intent.
--
--
--
-- /See:/ 'builtinIntentMetadata' smart constructor.
data BuiltinIntentMetadata = BuiltinIntentMetadata'
  { _bimSignature ::
      !(Maybe Text),
    _bimSupportedLocales :: !(Maybe [Locale])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BuiltinIntentMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bimSignature' - A unique identifier for the built-in intent. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
--
-- * 'bimSupportedLocales' - A list of identifiers for the locales that the intent supports.
builtinIntentMetadata ::
  BuiltinIntentMetadata
builtinIntentMetadata =
  BuiltinIntentMetadata'
    { _bimSignature = Nothing,
      _bimSupportedLocales = Nothing
    }

-- | A unique identifier for the built-in intent. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
bimSignature :: Lens' BuiltinIntentMetadata (Maybe Text)
bimSignature = lens _bimSignature (\s a -> s {_bimSignature = a})

-- | A list of identifiers for the locales that the intent supports.
bimSupportedLocales :: Lens' BuiltinIntentMetadata [Locale]
bimSupportedLocales = lens _bimSupportedLocales (\s a -> s {_bimSupportedLocales = a}) . _Default . _Coerce

instance FromJSON BuiltinIntentMetadata where
  parseJSON =
    withObject
      "BuiltinIntentMetadata"
      ( \x ->
          BuiltinIntentMetadata'
            <$> (x .:? "signature") <*> (x .:? "supportedLocales" .!= mempty)
      )

instance Hashable BuiltinIntentMetadata

instance NFData BuiltinIntentMetadata
