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
    pesPersonsWithRequiredEquipment,
    pesPersonsWithoutRequiredEquipment,
    pesPersonsIndeterminate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Summary information for required items of personal protective equipment (PPE) detected on persons by a call to 'DetectProtectiveEquipment' . You specify the required type of PPE in the @SummarizationAttributes@ ('ProtectiveEquipmentSummarizationAttributes' ) input parameter. The summary includes which persons were detected wearing the required personal protective equipment (@PersonsWithRequiredEquipment@ ), which persons were detected as not wearing the required PPE (@PersonsWithoutRequiredEquipment@ ), and the persons in which a determination could not be made (@PersonsIndeterminate@ ).
--
-- To get a total for each category, use the size of the field array. For example, to find out how many people were detected as wearing the specified PPE, use the size of the @PersonsWithRequiredEquipment@ array. If you want to find out more about a person, such as the location ('BoundingBox' ) of the person on the image, use the person ID in each array element. Each person ID matches the ID field of a 'ProtectiveEquipmentPerson' object returned in the @Persons@ array by @DetectProtectiveEquipment@ .
--
-- /See:/ 'mkProtectiveEquipmentSummary' smart constructor.
data ProtectiveEquipmentSummary = ProtectiveEquipmentSummary'
  { personsWithRequiredEquipment ::
      Lude.Maybe [Lude.Natural],
    personsWithoutRequiredEquipment ::
      Lude.Maybe [Lude.Natural],
    personsIndeterminate ::
      Lude.Maybe [Lude.Natural]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProtectiveEquipmentSummary' with the minimum fields required to make a request.
--
-- * 'personsIndeterminate' - An array of IDs for persons where it was not possible to determine if they are wearing personal protective equipment.
-- * 'personsWithRequiredEquipment' - An array of IDs for persons who are wearing detected personal protective equipment.
-- * 'personsWithoutRequiredEquipment' - An array of IDs for persons who are not wearing all of the types of PPE specified in the RequiredEquipmentTypes field of the detected personal protective equipment.
mkProtectiveEquipmentSummary ::
  ProtectiveEquipmentSummary
mkProtectiveEquipmentSummary =
  ProtectiveEquipmentSummary'
    { personsWithRequiredEquipment =
        Lude.Nothing,
      personsWithoutRequiredEquipment = Lude.Nothing,
      personsIndeterminate = Lude.Nothing
    }

-- | An array of IDs for persons who are wearing detected personal protective equipment.
--
-- /Note:/ Consider using 'personsWithRequiredEquipment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesPersonsWithRequiredEquipment :: Lens.Lens' ProtectiveEquipmentSummary (Lude.Maybe [Lude.Natural])
pesPersonsWithRequiredEquipment = Lens.lens (personsWithRequiredEquipment :: ProtectiveEquipmentSummary -> Lude.Maybe [Lude.Natural]) (\s a -> s {personsWithRequiredEquipment = a} :: ProtectiveEquipmentSummary)
{-# DEPRECATED pesPersonsWithRequiredEquipment "Use generic-lens or generic-optics with 'personsWithRequiredEquipment' instead." #-}

-- | An array of IDs for persons who are not wearing all of the types of PPE specified in the RequiredEquipmentTypes field of the detected personal protective equipment.
--
-- /Note:/ Consider using 'personsWithoutRequiredEquipment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesPersonsWithoutRequiredEquipment :: Lens.Lens' ProtectiveEquipmentSummary (Lude.Maybe [Lude.Natural])
pesPersonsWithoutRequiredEquipment = Lens.lens (personsWithoutRequiredEquipment :: ProtectiveEquipmentSummary -> Lude.Maybe [Lude.Natural]) (\s a -> s {personsWithoutRequiredEquipment = a} :: ProtectiveEquipmentSummary)
{-# DEPRECATED pesPersonsWithoutRequiredEquipment "Use generic-lens or generic-optics with 'personsWithoutRequiredEquipment' instead." #-}

-- | An array of IDs for persons where it was not possible to determine if they are wearing personal protective equipment.
--
-- /Note:/ Consider using 'personsIndeterminate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesPersonsIndeterminate :: Lens.Lens' ProtectiveEquipmentSummary (Lude.Maybe [Lude.Natural])
pesPersonsIndeterminate = Lens.lens (personsIndeterminate :: ProtectiveEquipmentSummary -> Lude.Maybe [Lude.Natural]) (\s a -> s {personsIndeterminate = a} :: ProtectiveEquipmentSummary)
{-# DEPRECATED pesPersonsIndeterminate "Use generic-lens or generic-optics with 'personsIndeterminate' instead." #-}

instance Lude.FromJSON ProtectiveEquipmentSummary where
  parseJSON =
    Lude.withObject
      "ProtectiveEquipmentSummary"
      ( \x ->
          ProtectiveEquipmentSummary'
            Lude.<$> (x Lude..:? "PersonsWithRequiredEquipment" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "PersonsWithoutRequiredEquipment" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "PersonsIndeterminate" Lude..!= Lude.mempty)
      )
