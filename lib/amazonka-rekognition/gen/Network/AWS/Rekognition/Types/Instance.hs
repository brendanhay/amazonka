{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Instance where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.BoundingBox

-- | An instance of a label returned by Amazon Rekognition Image ('DetectLabels' ) or by Amazon Rekognition Video ('GetLabelDetection' ).
--
--
--
-- /See:/ 'instance'' smart constructor.
data Instance = Instance'
  { _iBoundingBox :: !(Maybe BoundingBox),
    _iConfidence :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iBoundingBox' - The position of the label instance on the image.
--
-- * 'iConfidence' - The confidence that Amazon Rekognition has in the accuracy of the bounding box.
instance' ::
  Instance
instance' =
  Instance' {_iBoundingBox = Nothing, _iConfidence = Nothing}

-- | The position of the label instance on the image.
iBoundingBox :: Lens' Instance (Maybe BoundingBox)
iBoundingBox = lens _iBoundingBox (\s a -> s {_iBoundingBox = a})

-- | The confidence that Amazon Rekognition has in the accuracy of the bounding box.
iConfidence :: Lens' Instance (Maybe Double)
iConfidence = lens _iConfidence (\s a -> s {_iConfidence = a})

instance FromJSON Instance where
  parseJSON =
    withObject
      "Instance"
      ( \x ->
          Instance' <$> (x .:? "BoundingBox") <*> (x .:? "Confidence")
      )

instance Hashable Instance

instance NFData Instance
