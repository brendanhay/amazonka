{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.EyeOpen
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.EyeOpen where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates whether or not the eyes on the face are open, and the confidence level in the determination.
--
--
--
-- /See:/ 'eyeOpen' smart constructor.
data EyeOpen = EyeOpen'
  { _eoValue :: !(Maybe Bool),
    _eoConfidence :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EyeOpen' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eoValue' - Boolean value that indicates whether the eyes on the face are open.
--
-- * 'eoConfidence' - Level of confidence in the determination.
eyeOpen ::
  EyeOpen
eyeOpen = EyeOpen' {_eoValue = Nothing, _eoConfidence = Nothing}

-- | Boolean value that indicates whether the eyes on the face are open.
eoValue :: Lens' EyeOpen (Maybe Bool)
eoValue = lens _eoValue (\s a -> s {_eoValue = a})

-- | Level of confidence in the determination.
eoConfidence :: Lens' EyeOpen (Maybe Double)
eoConfidence = lens _eoConfidence (\s a -> s {_eoConfidence = a})

instance FromJSON EyeOpen where
  parseJSON =
    withObject
      "EyeOpen"
      (\x -> EyeOpen' <$> (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable EyeOpen

instance NFData EyeOpen
