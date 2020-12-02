{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Sunglasses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Sunglasses where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates whether or not the face is wearing sunglasses, and the confidence level in the determination.
--
--
--
-- /See:/ 'sunglasses' smart constructor.
data Sunglasses = Sunglasses'
  { _sValue :: !(Maybe Bool),
    _sConfidence :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Sunglasses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sValue' - Boolean value that indicates whether the face is wearing sunglasses or not.
--
-- * 'sConfidence' - Level of confidence in the determination.
sunglasses ::
  Sunglasses
sunglasses = Sunglasses' {_sValue = Nothing, _sConfidence = Nothing}

-- | Boolean value that indicates whether the face is wearing sunglasses or not.
sValue :: Lens' Sunglasses (Maybe Bool)
sValue = lens _sValue (\s a -> s {_sValue = a})

-- | Level of confidence in the determination.
sConfidence :: Lens' Sunglasses (Maybe Double)
sConfidence = lens _sConfidence (\s a -> s {_sConfidence = a})

instance FromJSON Sunglasses where
  parseJSON =
    withObject
      "Sunglasses"
      (\x -> Sunglasses' <$> (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable Sunglasses

instance NFData Sunglasses
