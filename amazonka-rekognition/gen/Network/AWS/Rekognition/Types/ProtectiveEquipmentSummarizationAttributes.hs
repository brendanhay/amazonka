{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProtectiveEquipmentSummarizationAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProtectiveEquipmentSummarizationAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.ProtectiveEquipmentType

-- | Specifies summary attributes to return from a call to
-- DetectProtectiveEquipment. You can specify which types of PPE to
-- summarize. You can also specify a minimum confidence value for
-- detections. Summary information is returned in the @Summary@
-- (ProtectiveEquipmentSummary) field of the response from
-- @DetectProtectiveEquipment@. The summary includes which persons in an
-- image were detected wearing the requested types of person protective
-- equipment (PPE), which persons were detected as not wearing PPE, and the
-- persons in which a determination could not be made. For more
-- information, see ProtectiveEquipmentSummary.
--
-- /See:/ 'newProtectiveEquipmentSummarizationAttributes' smart constructor.
data ProtectiveEquipmentSummarizationAttributes = ProtectiveEquipmentSummarizationAttributes'
  { -- | The minimum confidence level for which you want summary information. The
    -- confidence level applies to person detection, body part detection,
    -- equipment detection, and body part coverage. Amazon Rekognition doesn\'t
    -- return summary information with a confidence than this specified value.
    -- There isn\'t a default value.
    --
    -- Specify a @MinConfidence@ value that is between 50-100% as
    -- @DetectProtectiveEquipment@ returns predictions only where the detection
    -- confidence is between 50% - 100%. If you specify a value that is less
    -- than 50%, the results are the same specifying a value of 50%.
    minConfidence :: Prelude.Double,
    -- | An array of personal protective equipment types for which you want
    -- summary information. If a person is detected wearing a required
    -- requipment type, the person\'s ID is added to the
    -- @PersonsWithRequiredEquipment@ array field returned in
    -- ProtectiveEquipmentSummary by @DetectProtectiveEquipment@.
    requiredEquipmentTypes :: [ProtectiveEquipmentType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProtectiveEquipmentSummarizationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minConfidence', 'protectiveEquipmentSummarizationAttributes_minConfidence' - The minimum confidence level for which you want summary information. The
-- confidence level applies to person detection, body part detection,
-- equipment detection, and body part coverage. Amazon Rekognition doesn\'t
-- return summary information with a confidence than this specified value.
-- There isn\'t a default value.
--
-- Specify a @MinConfidence@ value that is between 50-100% as
-- @DetectProtectiveEquipment@ returns predictions only where the detection
-- confidence is between 50% - 100%. If you specify a value that is less
-- than 50%, the results are the same specifying a value of 50%.
--
-- 'requiredEquipmentTypes', 'protectiveEquipmentSummarizationAttributes_requiredEquipmentTypes' - An array of personal protective equipment types for which you want
-- summary information. If a person is detected wearing a required
-- requipment type, the person\'s ID is added to the
-- @PersonsWithRequiredEquipment@ array field returned in
-- ProtectiveEquipmentSummary by @DetectProtectiveEquipment@.
newProtectiveEquipmentSummarizationAttributes ::
  -- | 'minConfidence'
  Prelude.Double ->
  ProtectiveEquipmentSummarizationAttributes
newProtectiveEquipmentSummarizationAttributes
  pMinConfidence_ =
    ProtectiveEquipmentSummarizationAttributes'
      { minConfidence =
          pMinConfidence_,
        requiredEquipmentTypes =
          Prelude.mempty
      }

-- | The minimum confidence level for which you want summary information. The
-- confidence level applies to person detection, body part detection,
-- equipment detection, and body part coverage. Amazon Rekognition doesn\'t
-- return summary information with a confidence than this specified value.
-- There isn\'t a default value.
--
-- Specify a @MinConfidence@ value that is between 50-100% as
-- @DetectProtectiveEquipment@ returns predictions only where the detection
-- confidence is between 50% - 100%. If you specify a value that is less
-- than 50%, the results are the same specifying a value of 50%.
protectiveEquipmentSummarizationAttributes_minConfidence :: Lens.Lens' ProtectiveEquipmentSummarizationAttributes Prelude.Double
protectiveEquipmentSummarizationAttributes_minConfidence = Lens.lens (\ProtectiveEquipmentSummarizationAttributes' {minConfidence} -> minConfidence) (\s@ProtectiveEquipmentSummarizationAttributes' {} a -> s {minConfidence = a} :: ProtectiveEquipmentSummarizationAttributes)

-- | An array of personal protective equipment types for which you want
-- summary information. If a person is detected wearing a required
-- requipment type, the person\'s ID is added to the
-- @PersonsWithRequiredEquipment@ array field returned in
-- ProtectiveEquipmentSummary by @DetectProtectiveEquipment@.
protectiveEquipmentSummarizationAttributes_requiredEquipmentTypes :: Lens.Lens' ProtectiveEquipmentSummarizationAttributes [ProtectiveEquipmentType]
protectiveEquipmentSummarizationAttributes_requiredEquipmentTypes = Lens.lens (\ProtectiveEquipmentSummarizationAttributes' {requiredEquipmentTypes} -> requiredEquipmentTypes) (\s@ProtectiveEquipmentSummarizationAttributes' {} a -> s {requiredEquipmentTypes = a} :: ProtectiveEquipmentSummarizationAttributes) Prelude.. Prelude._Coerce

instance
  Prelude.Hashable
    ProtectiveEquipmentSummarizationAttributes

instance
  Prelude.NFData
    ProtectiveEquipmentSummarizationAttributes

instance
  Prelude.ToJSON
    ProtectiveEquipmentSummarizationAttributes
  where
  toJSON
    ProtectiveEquipmentSummarizationAttributes' {..} =
      Prelude.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("MinConfidence" Prelude..= minConfidence),
              Prelude.Just
                ( "RequiredEquipmentTypes"
                    Prelude..= requiredEquipmentTypes
                )
            ]
        )
