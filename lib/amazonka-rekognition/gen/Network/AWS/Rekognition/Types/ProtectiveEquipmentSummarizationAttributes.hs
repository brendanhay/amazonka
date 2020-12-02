{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProtectiveEquipmentSummarizationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProtectiveEquipmentSummarizationAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.ProtectiveEquipmentType

-- | Specifies summary attributes to return from a call to 'DetectProtectiveEquipment' . You can specify which types of PPE to summarize. You can also specify a minimum confidence value for detections. Summary information is returned in the @Summary@ ('ProtectiveEquipmentSummary' ) field of the response from @DetectProtectiveEquipment@ . The summary includes which persons in an image were detected wearing the requested types of person protective equipment (PPE), which persons were detected as not wearing PPE, and the persons in which a determination could not be made. For more information, see 'ProtectiveEquipmentSummary' .
--
--
--
-- /See:/ 'protectiveEquipmentSummarizationAttributes' smart constructor.
data ProtectiveEquipmentSummarizationAttributes = ProtectiveEquipmentSummarizationAttributes'
  { _pesaMinConfidence ::
      !Double,
    _pesaRequiredEquipmentTypes ::
      ![ProtectiveEquipmentType]
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ProtectiveEquipmentSummarizationAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pesaMinConfidence' - The minimum confidence level for which you want summary information. The confidence level applies to person detection, body part detection, equipment detection, and body part coverage. Amazon Rekognition doesn't return summary information with a confidence than this specified value. There isn't a default value. Specify a @MinConfidence@ value that is between 50-100% as @DetectProtectiveEquipment@ returns predictions only where the detection confidence is between 50% - 100%. If you specify a value that is less than 50%, the results are the same specifying a value of 50%.
--
-- * 'pesaRequiredEquipmentTypes' - An array of personal protective equipment types for which you want summary information. If a person is detected wearing a required requipment type, the person's ID is added to the @PersonsWithRequiredEquipment@ array field returned in 'ProtectiveEquipmentSummary' by @DetectProtectiveEquipment@ .
protectiveEquipmentSummarizationAttributes ::
  -- | 'pesaMinConfidence'
  Double ->
  ProtectiveEquipmentSummarizationAttributes
protectiveEquipmentSummarizationAttributes pMinConfidence_ =
  ProtectiveEquipmentSummarizationAttributes'
    { _pesaMinConfidence =
        pMinConfidence_,
      _pesaRequiredEquipmentTypes = mempty
    }

-- | The minimum confidence level for which you want summary information. The confidence level applies to person detection, body part detection, equipment detection, and body part coverage. Amazon Rekognition doesn't return summary information with a confidence than this specified value. There isn't a default value. Specify a @MinConfidence@ value that is between 50-100% as @DetectProtectiveEquipment@ returns predictions only where the detection confidence is between 50% - 100%. If you specify a value that is less than 50%, the results are the same specifying a value of 50%.
pesaMinConfidence :: Lens' ProtectiveEquipmentSummarizationAttributes Double
pesaMinConfidence = lens _pesaMinConfidence (\s a -> s {_pesaMinConfidence = a})

-- | An array of personal protective equipment types for which you want summary information. If a person is detected wearing a required requipment type, the person's ID is added to the @PersonsWithRequiredEquipment@ array field returned in 'ProtectiveEquipmentSummary' by @DetectProtectiveEquipment@ .
pesaRequiredEquipmentTypes :: Lens' ProtectiveEquipmentSummarizationAttributes [ProtectiveEquipmentType]
pesaRequiredEquipmentTypes = lens _pesaRequiredEquipmentTypes (\s a -> s {_pesaRequiredEquipmentTypes = a}) . _Coerce

instance Hashable ProtectiveEquipmentSummarizationAttributes

instance NFData ProtectiveEquipmentSummarizationAttributes

instance ToJSON ProtectiveEquipmentSummarizationAttributes where
  toJSON ProtectiveEquipmentSummarizationAttributes' {..} =
    object
      ( catMaybes
          [ Just ("MinConfidence" .= _pesaMinConfidence),
            Just ("RequiredEquipmentTypes" .= _pesaRequiredEquipmentTypes)
          ]
      )
