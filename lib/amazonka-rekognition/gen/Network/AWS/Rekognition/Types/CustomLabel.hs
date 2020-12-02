{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.CustomLabel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.CustomLabel where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.Geometry

-- | A custom label detected in an image by a call to 'DetectCustomLabels' .
--
--
--
-- /See:/ 'customLabel' smart constructor.
data CustomLabel = CustomLabel'
  { _clConfidence :: !(Maybe Double),
    _clName :: !(Maybe Text),
    _clGeometry :: !(Maybe Geometry)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomLabel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clConfidence' - The confidence that the model has in the detection of the custom label. The range is 0-100. A higher value indicates a higher confidence.
--
-- * 'clName' - The name of the custom label.
--
-- * 'clGeometry' - The location of the detected object on the image that corresponds to the custom label. Includes an axis aligned coarse bounding box surrounding the object and a finer grain polygon for more accurate spatial information.
customLabel ::
  CustomLabel
customLabel =
  CustomLabel'
    { _clConfidence = Nothing,
      _clName = Nothing,
      _clGeometry = Nothing
    }

-- | The confidence that the model has in the detection of the custom label. The range is 0-100. A higher value indicates a higher confidence.
clConfidence :: Lens' CustomLabel (Maybe Double)
clConfidence = lens _clConfidence (\s a -> s {_clConfidence = a})

-- | The name of the custom label.
clName :: Lens' CustomLabel (Maybe Text)
clName = lens _clName (\s a -> s {_clName = a})

-- | The location of the detected object on the image that corresponds to the custom label. Includes an axis aligned coarse bounding box surrounding the object and a finer grain polygon for more accurate spatial information.
clGeometry :: Lens' CustomLabel (Maybe Geometry)
clGeometry = lens _clGeometry (\s a -> s {_clGeometry = a})

instance FromJSON CustomLabel where
  parseJSON =
    withObject
      "CustomLabel"
      ( \x ->
          CustomLabel'
            <$> (x .:? "Confidence") <*> (x .:? "Name") <*> (x .:? "Geometry")
      )

instance Hashable CustomLabel

instance NFData CustomLabel
