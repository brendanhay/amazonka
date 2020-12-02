{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProtectiveEquipmentBodyPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProtectiveEquipmentBodyPart where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.BodyPart
import Network.AWS.Rekognition.Types.EquipmentDetection

-- | Information about a body part detected by 'DetectProtectiveEquipment' that contains PPE. An array of @ProtectiveEquipmentBodyPart@ objects is returned for each person detected by @DetectProtectiveEquipment@ .
--
--
--
-- /See:/ 'protectiveEquipmentBodyPart' smart constructor.
data ProtectiveEquipmentBodyPart = ProtectiveEquipmentBodyPart'
  { _pebpEquipmentDetections ::
      !(Maybe [EquipmentDetection]),
    _pebpConfidence :: !(Maybe Double),
    _pebpName :: !(Maybe BodyPart)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProtectiveEquipmentBodyPart' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pebpEquipmentDetections' - An array of Personal Protective Equipment items detected around a body part.
--
-- * 'pebpConfidence' - The confidence that Amazon Rekognition has in the detection accuracy of the detected body part.
--
-- * 'pebpName' - The detected body part.
protectiveEquipmentBodyPart ::
  ProtectiveEquipmentBodyPart
protectiveEquipmentBodyPart =
  ProtectiveEquipmentBodyPart'
    { _pebpEquipmentDetections = Nothing,
      _pebpConfidence = Nothing,
      _pebpName = Nothing
    }

-- | An array of Personal Protective Equipment items detected around a body part.
pebpEquipmentDetections :: Lens' ProtectiveEquipmentBodyPart [EquipmentDetection]
pebpEquipmentDetections = lens _pebpEquipmentDetections (\s a -> s {_pebpEquipmentDetections = a}) . _Default . _Coerce

-- | The confidence that Amazon Rekognition has in the detection accuracy of the detected body part.
pebpConfidence :: Lens' ProtectiveEquipmentBodyPart (Maybe Double)
pebpConfidence = lens _pebpConfidence (\s a -> s {_pebpConfidence = a})

-- | The detected body part.
pebpName :: Lens' ProtectiveEquipmentBodyPart (Maybe BodyPart)
pebpName = lens _pebpName (\s a -> s {_pebpName = a})

instance FromJSON ProtectiveEquipmentBodyPart where
  parseJSON =
    withObject
      "ProtectiveEquipmentBodyPart"
      ( \x ->
          ProtectiveEquipmentBodyPart'
            <$> (x .:? "EquipmentDetections" .!= mempty)
            <*> (x .:? "Confidence")
            <*> (x .:? "Name")
      )

instance Hashable ProtectiveEquipmentBodyPart

instance NFData ProtectiveEquipmentBodyPart
