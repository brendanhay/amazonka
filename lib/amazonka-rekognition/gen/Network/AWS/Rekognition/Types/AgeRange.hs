{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.AgeRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.AgeRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Structure containing the estimated age range, in years, for a face.
--
--
-- Amazon Rekognition estimates an age range for faces detected in the input image. Estimated age ranges can overlap. A face of a 5-year-old might have an estimated range of 4-6, while the face of a 6-year-old might have an estimated range of 4-8.
--
--
-- /See:/ 'ageRange' smart constructor.
data AgeRange = AgeRange'
  { _arLow :: !(Maybe Nat),
    _arHigh :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AgeRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arLow' - The lowest estimated age.
--
-- * 'arHigh' - The highest estimated age.
ageRange ::
  AgeRange
ageRange = AgeRange' {_arLow = Nothing, _arHigh = Nothing}

-- | The lowest estimated age.
arLow :: Lens' AgeRange (Maybe Natural)
arLow = lens _arLow (\s a -> s {_arLow = a}) . mapping _Nat

-- | The highest estimated age.
arHigh :: Lens' AgeRange (Maybe Natural)
arHigh = lens _arHigh (\s a -> s {_arHigh = a}) . mapping _Nat

instance FromJSON AgeRange where
  parseJSON =
    withObject
      "AgeRange"
      (\x -> AgeRange' <$> (x .:? "Low") <*> (x .:? "High"))

instance Hashable AgeRange

instance NFData AgeRange
