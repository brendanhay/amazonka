{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Eyeglasses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Eyeglasses where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates whether or not the face is wearing eye glasses, and the confidence level in the determination.
--
--
--
-- /See:/ 'eyeglasses' smart constructor.
data Eyeglasses = Eyeglasses'
  { _eyeValue :: !(Maybe Bool),
    _eyeConfidence :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Eyeglasses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eyeValue' - Boolean value that indicates whether the face is wearing eye glasses or not.
--
-- * 'eyeConfidence' - Level of confidence in the determination.
eyeglasses ::
  Eyeglasses
eyeglasses =
  Eyeglasses' {_eyeValue = Nothing, _eyeConfidence = Nothing}

-- | Boolean value that indicates whether the face is wearing eye glasses or not.
eyeValue :: Lens' Eyeglasses (Maybe Bool)
eyeValue = lens _eyeValue (\s a -> s {_eyeValue = a})

-- | Level of confidence in the determination.
eyeConfidence :: Lens' Eyeglasses (Maybe Double)
eyeConfidence = lens _eyeConfidence (\s a -> s {_eyeConfidence = a})

instance FromJSON Eyeglasses where
  parseJSON =
    withObject
      "Eyeglasses"
      (\x -> Eyeglasses' <$> (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable Eyeglasses

instance NFData Eyeglasses
