{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.ModelSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.ModelSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The object used to call your custom language model to your transcription job.
--
--
--
-- /See:/ 'modelSettings' smart constructor.
newtype ModelSettings = ModelSettings'
  { _msLanguageModelName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModelSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msLanguageModelName' - The name of your custom language model.
modelSettings ::
  ModelSettings
modelSettings = ModelSettings' {_msLanguageModelName = Nothing}

-- | The name of your custom language model.
msLanguageModelName :: Lens' ModelSettings (Maybe Text)
msLanguageModelName = lens _msLanguageModelName (\s a -> s {_msLanguageModelName = a})

instance FromJSON ModelSettings where
  parseJSON =
    withObject
      "ModelSettings"
      (\x -> ModelSettings' <$> (x .:? "LanguageModelName"))

instance Hashable ModelSettings

instance NFData ModelSettings

instance ToJSON ModelSettings where
  toJSON ModelSettings' {..} =
    object
      (catMaybes [("LanguageModelName" .=) <$> _msLanguageModelName])
