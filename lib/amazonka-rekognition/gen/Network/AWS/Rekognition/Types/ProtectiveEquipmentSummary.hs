{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProtectiveEquipmentSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProtectiveEquipmentSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Summary information for required items of personal protective equipment (PPE) detected on persons by a call to 'DetectProtectiveEquipment' . You specify the required type of PPE in the @SummarizationAttributes@ ('ProtectiveEquipmentSummarizationAttributes' ) input parameter. The summary includes which persons were detected wearing the required personal protective equipment (@PersonsWithRequiredEquipment@ ), which persons were detected as not wearing the required PPE (@PersonsWithoutRequiredEquipment@ ), and the persons in which a determination could not be made (@PersonsIndeterminate@ ).
--
--
-- To get a total for each category, use the size of the field array. For example, to find out how many people were detected as wearing the specified PPE, use the size of the @PersonsWithRequiredEquipment@ array. If you want to find out more about a person, such as the location ('BoundingBox' ) of the person on the image, use the person ID in each array element. Each person ID matches the ID field of a 'ProtectiveEquipmentPerson' object returned in the @Persons@ array by @DetectProtectiveEquipment@ .
--
--
-- /See:/ 'protectiveEquipmentSummary' smart constructor.
data ProtectiveEquipmentSummary = ProtectiveEquipmentSummary'
  { _pesPersonsWithRequiredEquipment ::
      !(Maybe [Nat]),
    _pesPersonsWithoutRequiredEquipment ::
      !(Maybe [Nat]),
    _pesPersonsIndeterminate ::
      !(Maybe [Nat])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProtectiveEquipmentSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pesPersonsWithRequiredEquipment' - An array of IDs for persons who are wearing detected personal protective equipment.
--
-- * 'pesPersonsWithoutRequiredEquipment' - An array of IDs for persons who are not wearing all of the types of PPE specified in the RequiredEquipmentTypes field of the detected personal protective equipment.
--
-- * 'pesPersonsIndeterminate' - An array of IDs for persons where it was not possible to determine if they are wearing personal protective equipment.
protectiveEquipmentSummary ::
  ProtectiveEquipmentSummary
protectiveEquipmentSummary =
  ProtectiveEquipmentSummary'
    { _pesPersonsWithRequiredEquipment =
        Nothing,
      _pesPersonsWithoutRequiredEquipment = Nothing,
      _pesPersonsIndeterminate = Nothing
    }

-- | An array of IDs for persons who are wearing detected personal protective equipment.
pesPersonsWithRequiredEquipment :: Lens' ProtectiveEquipmentSummary [Natural]
pesPersonsWithRequiredEquipment = lens _pesPersonsWithRequiredEquipment (\s a -> s {_pesPersonsWithRequiredEquipment = a}) . _Default . _Coerce

-- | An array of IDs for persons who are not wearing all of the types of PPE specified in the RequiredEquipmentTypes field of the detected personal protective equipment.
pesPersonsWithoutRequiredEquipment :: Lens' ProtectiveEquipmentSummary [Natural]
pesPersonsWithoutRequiredEquipment = lens _pesPersonsWithoutRequiredEquipment (\s a -> s {_pesPersonsWithoutRequiredEquipment = a}) . _Default . _Coerce

-- | An array of IDs for persons where it was not possible to determine if they are wearing personal protective equipment.
pesPersonsIndeterminate :: Lens' ProtectiveEquipmentSummary [Natural]
pesPersonsIndeterminate = lens _pesPersonsIndeterminate (\s a -> s {_pesPersonsIndeterminate = a}) . _Default . _Coerce

instance FromJSON ProtectiveEquipmentSummary where
  parseJSON =
    withObject
      "ProtectiveEquipmentSummary"
      ( \x ->
          ProtectiveEquipmentSummary'
            <$> (x .:? "PersonsWithRequiredEquipment" .!= mempty)
            <*> (x .:? "PersonsWithoutRequiredEquipment" .!= mempty)
            <*> (x .:? "PersonsIndeterminate" .!= mempty)
      )

instance Hashable ProtectiveEquipmentSummary

instance NFData ProtectiveEquipmentSummary
