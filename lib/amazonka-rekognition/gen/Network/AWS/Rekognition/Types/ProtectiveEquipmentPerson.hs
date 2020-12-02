{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProtectiveEquipmentPerson
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProtectiveEquipmentPerson where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.ProtectiveEquipmentBodyPart

-- | A person detected by a call to 'DetectProtectiveEquipment' . The API returns all persons detected in the input image in an array of @ProtectiveEquipmentPerson@ objects.
--
--
--
-- /See:/ 'protectiveEquipmentPerson' smart constructor.
data ProtectiveEquipmentPerson = ProtectiveEquipmentPerson'
  { _pepBodyParts ::
      !(Maybe [ProtectiveEquipmentBodyPart]),
    _pepBoundingBox :: !(Maybe BoundingBox),
    _pepConfidence :: !(Maybe Double),
    _pepId :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProtectiveEquipmentPerson' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pepBodyParts' - An array of body parts detected on a person's body (including body parts without PPE).
--
-- * 'pepBoundingBox' - A bounding box around the detected person.
--
-- * 'pepConfidence' - The confidence that Amazon Rekognition has that the bounding box contains a person.
--
-- * 'pepId' - The identifier for the detected person. The identifier is only unique for a single call to @DetectProtectiveEquipment@ .
protectiveEquipmentPerson ::
  ProtectiveEquipmentPerson
protectiveEquipmentPerson =
  ProtectiveEquipmentPerson'
    { _pepBodyParts = Nothing,
      _pepBoundingBox = Nothing,
      _pepConfidence = Nothing,
      _pepId = Nothing
    }

-- | An array of body parts detected on a person's body (including body parts without PPE).
pepBodyParts :: Lens' ProtectiveEquipmentPerson [ProtectiveEquipmentBodyPart]
pepBodyParts = lens _pepBodyParts (\s a -> s {_pepBodyParts = a}) . _Default . _Coerce

-- | A bounding box around the detected person.
pepBoundingBox :: Lens' ProtectiveEquipmentPerson (Maybe BoundingBox)
pepBoundingBox = lens _pepBoundingBox (\s a -> s {_pepBoundingBox = a})

-- | The confidence that Amazon Rekognition has that the bounding box contains a person.
pepConfidence :: Lens' ProtectiveEquipmentPerson (Maybe Double)
pepConfidence = lens _pepConfidence (\s a -> s {_pepConfidence = a})

-- | The identifier for the detected person. The identifier is only unique for a single call to @DetectProtectiveEquipment@ .
pepId :: Lens' ProtectiveEquipmentPerson (Maybe Natural)
pepId = lens _pepId (\s a -> s {_pepId = a}) . mapping _Nat

instance FromJSON ProtectiveEquipmentPerson where
  parseJSON =
    withObject
      "ProtectiveEquipmentPerson"
      ( \x ->
          ProtectiveEquipmentPerson'
            <$> (x .:? "BodyParts" .!= mempty)
            <*> (x .:? "BoundingBox")
            <*> (x .:? "Confidence")
            <*> (x .:? "Id")
      )

instance Hashable ProtectiveEquipmentPerson

instance NFData ProtectiveEquipmentPerson
