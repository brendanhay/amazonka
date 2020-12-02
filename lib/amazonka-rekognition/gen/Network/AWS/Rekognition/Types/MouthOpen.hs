{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.MouthOpen
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.MouthOpen where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates whether or not the mouth on the face is open, and the confidence level in the determination.
--
--
--
-- /See:/ 'mouthOpen' smart constructor.
data MouthOpen = MouthOpen'
  { _moValue :: !(Maybe Bool),
    _moConfidence :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MouthOpen' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'moValue' - Boolean value that indicates whether the mouth on the face is open or not.
--
-- * 'moConfidence' - Level of confidence in the determination.
mouthOpen ::
  MouthOpen
mouthOpen = MouthOpen' {_moValue = Nothing, _moConfidence = Nothing}

-- | Boolean value that indicates whether the mouth on the face is open or not.
moValue :: Lens' MouthOpen (Maybe Bool)
moValue = lens _moValue (\s a -> s {_moValue = a})

-- | Level of confidence in the determination.
moConfidence :: Lens' MouthOpen (Maybe Double)
moConfidence = lens _moConfidence (\s a -> s {_moConfidence = a})

instance FromJSON MouthOpen where
  parseJSON =
    withObject
      "MouthOpen"
      (\x -> MouthOpen' <$> (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable MouthOpen

instance NFData MouthOpen
