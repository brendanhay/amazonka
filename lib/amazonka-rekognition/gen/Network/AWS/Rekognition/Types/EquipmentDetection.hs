{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.EquipmentDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.EquipmentDetection where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.CoversBodyPart
import Network.AWS.Rekognition.Types.ProtectiveEquipmentType

-- | Information about an item of Personal Protective Equipment (PPE) detected by 'DetectProtectiveEquipment' . For more information, see 'DetectProtectiveEquipment' .
--
--
--
-- /See:/ 'equipmentDetection' smart constructor.
data EquipmentDetection = EquipmentDetection'
  { _edBoundingBox ::
      !(Maybe BoundingBox),
    _edCoversBodyPart :: !(Maybe CoversBodyPart),
    _edConfidence :: !(Maybe Double),
    _edType :: !(Maybe ProtectiveEquipmentType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EquipmentDetection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edBoundingBox' - A bounding box surrounding the item of detected PPE.
--
-- * 'edCoversBodyPart' - Information about the body part covered by the detected PPE.
--
-- * 'edConfidence' - The confidence that Amazon Rekognition has that the bounding box (@BoundingBox@ ) contains an item of PPE.
--
-- * 'edType' - The type of detected PPE.
equipmentDetection ::
  EquipmentDetection
equipmentDetection =
  EquipmentDetection'
    { _edBoundingBox = Nothing,
      _edCoversBodyPart = Nothing,
      _edConfidence = Nothing,
      _edType = Nothing
    }

-- | A bounding box surrounding the item of detected PPE.
edBoundingBox :: Lens' EquipmentDetection (Maybe BoundingBox)
edBoundingBox = lens _edBoundingBox (\s a -> s {_edBoundingBox = a})

-- | Information about the body part covered by the detected PPE.
edCoversBodyPart :: Lens' EquipmentDetection (Maybe CoversBodyPart)
edCoversBodyPart = lens _edCoversBodyPart (\s a -> s {_edCoversBodyPart = a})

-- | The confidence that Amazon Rekognition has that the bounding box (@BoundingBox@ ) contains an item of PPE.
edConfidence :: Lens' EquipmentDetection (Maybe Double)
edConfidence = lens _edConfidence (\s a -> s {_edConfidence = a})

-- | The type of detected PPE.
edType :: Lens' EquipmentDetection (Maybe ProtectiveEquipmentType)
edType = lens _edType (\s a -> s {_edType = a})

instance FromJSON EquipmentDetection where
  parseJSON =
    withObject
      "EquipmentDetection"
      ( \x ->
          EquipmentDetection'
            <$> (x .:? "BoundingBox")
            <*> (x .:? "CoversBodyPart")
            <*> (x .:? "Confidence")
            <*> (x .:? "Type")
      )

instance Hashable EquipmentDetection

instance NFData EquipmentDetection
