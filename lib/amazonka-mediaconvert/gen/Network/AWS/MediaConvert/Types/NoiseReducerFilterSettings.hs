{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducerFilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NoiseReducerFilterSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for a noise reducer filter
--
-- /See:/ 'noiseReducerFilterSettings' smart constructor.
newtype NoiseReducerFilterSettings = NoiseReducerFilterSettings'
  { _nrfsStrength ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NoiseReducerFilterSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nrfsStrength' - Relative strength of noise reducing filter. Higher values produce stronger filtering.
noiseReducerFilterSettings ::
  NoiseReducerFilterSettings
noiseReducerFilterSettings =
  NoiseReducerFilterSettings' {_nrfsStrength = Nothing}

-- | Relative strength of noise reducing filter. Higher values produce stronger filtering.
nrfsStrength :: Lens' NoiseReducerFilterSettings (Maybe Natural)
nrfsStrength = lens _nrfsStrength (\s a -> s {_nrfsStrength = a}) . mapping _Nat

instance FromJSON NoiseReducerFilterSettings where
  parseJSON =
    withObject
      "NoiseReducerFilterSettings"
      (\x -> NoiseReducerFilterSettings' <$> (x .:? "strength"))

instance Hashable NoiseReducerFilterSettings

instance NFData NoiseReducerFilterSettings

instance ToJSON NoiseReducerFilterSettings where
  toJSON NoiseReducerFilterSettings' {..} =
    object (catMaybes [("strength" .=) <$> _nrfsStrength])
