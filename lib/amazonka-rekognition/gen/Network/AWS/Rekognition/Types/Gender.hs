{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Gender
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Gender where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.GenderType

-- | The predicted gender of a detected face.
--
--
-- Amazon Rekognition makes gender binary (male/female) predictions based on the physical appearance of a face in a particular image. This kind of prediction is not designed to categorize a person’s gender identity, and you shouldn't use Amazon Rekognition to make such a determination. For example, a male actor wearing a long-haired wig and earrings for a role might be predicted as female.
--
-- Using Amazon Rekognition to make gender binary predictions is best suited for use cases where aggregate gender distribution statistics need to be analyzed without identifying specific users. For example, the percentage of female users compared to male users on a social media platform.
--
-- We don't recommend using gender binary predictions to make decisions that impact  an individual's rights, privacy, or access to services.
--
--
-- /See:/ 'gender' smart constructor.
data Gender = Gender'
  { _gValue :: !(Maybe GenderType),
    _gConfidence :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Gender' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gValue' - The predicted gender of the face.
--
-- * 'gConfidence' - Level of confidence in the prediction.
gender ::
  Gender
gender = Gender' {_gValue = Nothing, _gConfidence = Nothing}

-- | The predicted gender of the face.
gValue :: Lens' Gender (Maybe GenderType)
gValue = lens _gValue (\s a -> s {_gValue = a})

-- | Level of confidence in the prediction.
gConfidence :: Lens' Gender (Maybe Double)
gConfidence = lens _gConfidence (\s a -> s {_gConfidence = a})

instance FromJSON Gender where
  parseJSON =
    withObject
      "Gender"
      (\x -> Gender' <$> (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable Gender

instance NFData Gender
