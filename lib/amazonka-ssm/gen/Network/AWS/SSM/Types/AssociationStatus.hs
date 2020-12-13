{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationStatus
  ( AssociationStatus (..),

    -- * Smart constructor
    mkAssociationStatus,

    -- * Lenses
    asAdditionalInfo,
    asDate,
    asName,
    asMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.AssociationStatusName

-- | Describes an association status.
--
-- /See:/ 'mkAssociationStatus' smart constructor.
data AssociationStatus = AssociationStatus'
  { -- | A user-defined string.
    additionalInfo :: Lude.Maybe Lude.Text,
    -- | The date when the status changed.
    date :: Lude.Timestamp,
    -- | The status.
    name :: AssociationStatusName,
    -- | The reason for the status.
    message :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociationStatus' with the minimum fields required to make a request.
--
-- * 'additionalInfo' - A user-defined string.
-- * 'date' - The date when the status changed.
-- * 'name' - The status.
-- * 'message' - The reason for the status.
mkAssociationStatus ::
  -- | 'date'
  Lude.Timestamp ->
  -- | 'name'
  AssociationStatusName ->
  -- | 'message'
  Lude.Text ->
  AssociationStatus
mkAssociationStatus pDate_ pName_ pMessage_ =
  AssociationStatus'
    { additionalInfo = Lude.Nothing,
      date = pDate_,
      name = pName_,
      message = pMessage_
    }

-- | A user-defined string.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAdditionalInfo :: Lens.Lens' AssociationStatus (Lude.Maybe Lude.Text)
asAdditionalInfo = Lens.lens (additionalInfo :: AssociationStatus -> Lude.Maybe Lude.Text) (\s a -> s {additionalInfo = a} :: AssociationStatus)
{-# DEPRECATED asAdditionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead." #-}

-- | The date when the status changed.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asDate :: Lens.Lens' AssociationStatus Lude.Timestamp
asDate = Lens.lens (date :: AssociationStatus -> Lude.Timestamp) (\s a -> s {date = a} :: AssociationStatus)
{-# DEPRECATED asDate "Use generic-lens or generic-optics with 'date' instead." #-}

-- | The status.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asName :: Lens.Lens' AssociationStatus AssociationStatusName
asName = Lens.lens (name :: AssociationStatus -> AssociationStatusName) (\s a -> s {name = a} :: AssociationStatus)
{-# DEPRECATED asName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The reason for the status.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asMessage :: Lens.Lens' AssociationStatus Lude.Text
asMessage = Lens.lens (message :: AssociationStatus -> Lude.Text) (\s a -> s {message = a} :: AssociationStatus)
{-# DEPRECATED asMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON AssociationStatus where
  parseJSON =
    Lude.withObject
      "AssociationStatus"
      ( \x ->
          AssociationStatus'
            Lude.<$> (x Lude..:? "AdditionalInfo")
            Lude.<*> (x Lude..: "Date")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..: "Message")
      )

instance Lude.ToJSON AssociationStatus where
  toJSON AssociationStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AdditionalInfo" Lude..=) Lude.<$> additionalInfo,
            Lude.Just ("Date" Lude..= date),
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Message" Lude..= message)
          ]
      )
