{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Intent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.Intent where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifies the specific version of an intent.
--
--
--
-- /See:/ 'intent' smart constructor.
data Intent = Intent'
  { _iIntentName :: !Text,
    _iIntentVersion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Intent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iIntentName' - The name of the intent.
--
-- * 'iIntentVersion' - The version of the intent.
intent ::
  -- | 'iIntentName'
  Text ->
  -- | 'iIntentVersion'
  Text ->
  Intent
intent pIntentName_ pIntentVersion_ =
  Intent'
    { _iIntentName = pIntentName_,
      _iIntentVersion = pIntentVersion_
    }

-- | The name of the intent.
iIntentName :: Lens' Intent Text
iIntentName = lens _iIntentName (\s a -> s {_iIntentName = a})

-- | The version of the intent.
iIntentVersion :: Lens' Intent Text
iIntentVersion = lens _iIntentVersion (\s a -> s {_iIntentVersion = a})

instance FromJSON Intent where
  parseJSON =
    withObject
      "Intent"
      (\x -> Intent' <$> (x .: "intentName") <*> (x .: "intentVersion"))

instance Hashable Intent

instance NFData Intent

instance ToJSON Intent where
  toJSON Intent' {..} =
    object
      ( catMaybes
          [ Just ("intentName" .= _iIntentName),
            Just ("intentVersion" .= _iIntentVersion)
          ]
      )
