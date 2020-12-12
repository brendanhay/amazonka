{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemNotification
  ( OpsItemNotification (..),

    -- * Smart constructor
    mkOpsItemNotification,

    -- * Lenses
    oinARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A notification about the OpsItem.
--
-- /See:/ 'mkOpsItemNotification' smart constructor.
newtype OpsItemNotification = OpsItemNotification'
  { arn ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OpsItemNotification' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
mkOpsItemNotification ::
  OpsItemNotification
mkOpsItemNotification = OpsItemNotification' {arn = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oinARN :: Lens.Lens' OpsItemNotification (Lude.Maybe Lude.Text)
oinARN = Lens.lens (arn :: OpsItemNotification -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: OpsItemNotification)
{-# DEPRECATED oinARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.FromJSON OpsItemNotification where
  parseJSON =
    Lude.withObject
      "OpsItemNotification"
      (\x -> OpsItemNotification' Lude.<$> (x Lude..:? "Arn"))

instance Lude.ToJSON OpsItemNotification where
  toJSON OpsItemNotification' {..} =
    Lude.object (Lude.catMaybes [("Arn" Lude..=) Lude.<$> arn])
