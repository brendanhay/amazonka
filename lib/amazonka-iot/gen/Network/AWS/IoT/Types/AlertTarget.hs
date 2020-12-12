{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AlertTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AlertTarget
  ( AlertTarget (..),

    -- * Smart constructor
    mkAlertTarget,

    -- * Lenses
    atAlertTargetARN,
    atRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure containing the alert target ARN and the role ARN.
--
-- /See:/ 'mkAlertTarget' smart constructor.
data AlertTarget = AlertTarget'
  { alertTargetARN :: Lude.Text,
    roleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AlertTarget' with the minimum fields required to make a request.
--
-- * 'alertTargetARN' - The ARN of the notification target to which alerts are sent.
-- * 'roleARN' - The ARN of the role that grants permission to send alerts to the notification target.
mkAlertTarget ::
  -- | 'alertTargetARN'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  AlertTarget
mkAlertTarget pAlertTargetARN_ pRoleARN_ =
  AlertTarget'
    { alertTargetARN = pAlertTargetARN_,
      roleARN = pRoleARN_
    }

-- | The ARN of the notification target to which alerts are sent.
--
-- /Note:/ Consider using 'alertTargetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atAlertTargetARN :: Lens.Lens' AlertTarget Lude.Text
atAlertTargetARN = Lens.lens (alertTargetARN :: AlertTarget -> Lude.Text) (\s a -> s {alertTargetARN = a} :: AlertTarget)
{-# DEPRECATED atAlertTargetARN "Use generic-lens or generic-optics with 'alertTargetARN' instead." #-}

-- | The ARN of the role that grants permission to send alerts to the notification target.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atRoleARN :: Lens.Lens' AlertTarget Lude.Text
atRoleARN = Lens.lens (roleARN :: AlertTarget -> Lude.Text) (\s a -> s {roleARN = a} :: AlertTarget)
{-# DEPRECATED atRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON AlertTarget where
  parseJSON =
    Lude.withObject
      "AlertTarget"
      ( \x ->
          AlertTarget'
            Lude.<$> (x Lude..: "alertTargetArn") Lude.<*> (x Lude..: "roleArn")
      )

instance Lude.ToJSON AlertTarget where
  toJSON AlertTarget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("alertTargetArn" Lude..= alertTargetARN),
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
