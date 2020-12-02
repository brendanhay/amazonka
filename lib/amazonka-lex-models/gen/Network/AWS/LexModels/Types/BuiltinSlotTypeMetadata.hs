{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.BuiltinSlotTypeMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.BuiltinSlotTypeMetadata where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.Locale
import Network.AWS.Prelude

-- | Provides information about a built in slot type.
--
--
--
-- /See:/ 'builtinSlotTypeMetadata' smart constructor.
data BuiltinSlotTypeMetadata = BuiltinSlotTypeMetadata'
  { _bstmSignature ::
      !(Maybe Text),
    _bstmSupportedLocales :: !(Maybe [Locale])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BuiltinSlotTypeMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bstmSignature' - A unique identifier for the built-in slot type. To find the signature for a slot type, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
--
-- * 'bstmSupportedLocales' - A list of target locales for the slot.
builtinSlotTypeMetadata ::
  BuiltinSlotTypeMetadata
builtinSlotTypeMetadata =
  BuiltinSlotTypeMetadata'
    { _bstmSignature = Nothing,
      _bstmSupportedLocales = Nothing
    }

-- | A unique identifier for the built-in slot type. To find the signature for a slot type, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
bstmSignature :: Lens' BuiltinSlotTypeMetadata (Maybe Text)
bstmSignature = lens _bstmSignature (\s a -> s {_bstmSignature = a})

-- | A list of target locales for the slot.
bstmSupportedLocales :: Lens' BuiltinSlotTypeMetadata [Locale]
bstmSupportedLocales = lens _bstmSupportedLocales (\s a -> s {_bstmSupportedLocales = a}) . _Default . _Coerce

instance FromJSON BuiltinSlotTypeMetadata where
  parseJSON =
    withObject
      "BuiltinSlotTypeMetadata"
      ( \x ->
          BuiltinSlotTypeMetadata'
            <$> (x .:? "signature") <*> (x .:? "supportedLocales" .!= mempty)
      )

instance Hashable BuiltinSlotTypeMetadata

instance NFData BuiltinSlotTypeMetadata
