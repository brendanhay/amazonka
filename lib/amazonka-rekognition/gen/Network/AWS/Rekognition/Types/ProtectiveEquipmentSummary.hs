{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProtectiveEquipmentSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProtectiveEquipmentSummary
  ( ProtectiveEquipmentSummary (..),

    -- * Smart constructor
    mkProtectiveEquipmentSummary,

    -- * Lenses
    pesPersonsIndeterminate,
    pesPersonsWithRequiredEquipment,
    pesPersonsWithoutRequiredEquipment,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Summary information for required items of personal protective equipment (PPE) detected on persons by a call to 'DetectProtectiveEquipment' . You specify the required type of PPE in the @SummarizationAttributes@ ('ProtectiveEquipmentSummarizationAttributes' ) input parameter. The summary includes which persons were detected wearing the required personal protective equipment (@PersonsWithRequiredEquipment@ ), which persons were detected as not wearing the required PPE (@PersonsWithoutRequiredEquipment@ ), and the persons in which a determination could not be made (@PersonsIndeterminate@ ).
--
-- To get a total for each category, use the size of the field array. For example, to find out how many people were detected as wearing the specified PPE, use the size of the @PersonsWithRequiredEquipment@ array. If you want to find out more about a person, such as the location ('BoundingBox' ) of the person on the image, use the person ID in each array element. Each person ID matches the ID field of a 'ProtectiveEquipmentPerson' object returned in the @Persons@ array by @DetectProtectiveEquipment@ .
--
-- /See:/ 'mkProtectiveEquipmentSummary' smart constructor.
data ProtectiveEquipmentSummary = ProtectiveEquipmentSummary'
  { -- | An array of IDs for persons where it was not possible to determine if they are wearing personal protective equipment.
    personsIndeterminate :: Core.Maybe [Core.Natural],
    -- | An array of IDs for persons who are wearing detected personal protective equipment.
    personsWithRequiredEquipment :: Core.Maybe [Core.Natural],
    -- | An array of IDs for persons who are not wearing all of the types of PPE specified in the RequiredEquipmentTypes field of the detected personal protective equipment.
    personsWithoutRequiredEquipment :: Core.Maybe [Core.Natural]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProtectiveEquipmentSummary' value with any optional fields omitted.
mkProtectiveEquipmentSummary ::
  ProtectiveEquipmentSummary
mkProtectiveEquipmentSummary =
  ProtectiveEquipmentSummary'
    { personsIndeterminate = Core.Nothing,
      personsWithRequiredEquipment = Core.Nothing,
      personsWithoutRequiredEquipment = Core.Nothing
    }

-- | An array of IDs for persons where it was not possible to determine if they are wearing personal protective equipment.
--
-- /Note:/ Consider using 'personsIndeterminate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesPersonsIndeterminate :: Lens.Lens' ProtectiveEquipmentSummary (Core.Maybe [Core.Natural])
pesPersonsIndeterminate = Lens.field @"personsIndeterminate"
{-# DEPRECATED pesPersonsIndeterminate "Use generic-lens or generic-optics with 'personsIndeterminate' instead." #-}

-- | An array of IDs for persons who are wearing detected personal protective equipment.
--
-- /Note:/ Consider using 'personsWithRequiredEquipment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesPersonsWithRequiredEquipment :: Lens.Lens' ProtectiveEquipmentSummary (Core.Maybe [Core.Natural])
pesPersonsWithRequiredEquipment = Lens.field @"personsWithRequiredEquipment"
{-# DEPRECATED pesPersonsWithRequiredEquipment "Use generic-lens or generic-optics with 'personsWithRequiredEquipment' instead." #-}

-- | An array of IDs for persons who are not wearing all of the types of PPE specified in the RequiredEquipmentTypes field of the detected personal protective equipment.
--
-- /Note:/ Consider using 'personsWithoutRequiredEquipment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesPersonsWithoutRequiredEquipment :: Lens.Lens' ProtectiveEquipmentSummary (Core.Maybe [Core.Natural])
pesPersonsWithoutRequiredEquipment = Lens.field @"personsWithoutRequiredEquipment"
{-# DEPRECATED pesPersonsWithoutRequiredEquipment "Use generic-lens or generic-optics with 'personsWithoutRequiredEquipment' instead." #-}

instance Core.FromJSON ProtectiveEquipmentSummary where
  parseJSON =
    Core.withObject "ProtectiveEquipmentSummary" Core.$
      \x ->
        ProtectiveEquipmentSummary'
          Core.<$> (x Core..:? "PersonsIndeterminate")
          Core.<*> (x Core..:? "PersonsWithRequiredEquipment")
          Core.<*> (x Core..:? "PersonsWithoutRequiredEquipment")
