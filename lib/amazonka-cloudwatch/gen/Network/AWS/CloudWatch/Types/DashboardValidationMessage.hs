{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.DashboardValidationMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.DashboardValidationMessage
  ( DashboardValidationMessage (..),

    -- * Smart constructor
    mkDashboardValidationMessage,

    -- * Lenses
    dvmDataPath,
    dvmMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An error or warning for the operation.
--
-- /See:/ 'mkDashboardValidationMessage' smart constructor.
data DashboardValidationMessage = DashboardValidationMessage'
  { -- | The data path related to the message.
    dataPath :: Lude.Maybe Lude.Text,
    -- | A message describing the error or warning.
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DashboardValidationMessage' with the minimum fields required to make a request.
--
-- * 'dataPath' - The data path related to the message.
-- * 'message' - A message describing the error or warning.
mkDashboardValidationMessage ::
  DashboardValidationMessage
mkDashboardValidationMessage =
  DashboardValidationMessage'
    { dataPath = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The data path related to the message.
--
-- /Note:/ Consider using 'dataPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmDataPath :: Lens.Lens' DashboardValidationMessage (Lude.Maybe Lude.Text)
dvmDataPath = Lens.lens (dataPath :: DashboardValidationMessage -> Lude.Maybe Lude.Text) (\s a -> s {dataPath = a} :: DashboardValidationMessage)
{-# DEPRECATED dvmDataPath "Use generic-lens or generic-optics with 'dataPath' instead." #-}

-- | A message describing the error or warning.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmMessage :: Lens.Lens' DashboardValidationMessage (Lude.Maybe Lude.Text)
dvmMessage = Lens.lens (message :: DashboardValidationMessage -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: DashboardValidationMessage)
{-# DEPRECATED dvmMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML DashboardValidationMessage where
  parseXML x =
    DashboardValidationMessage'
      Lude.<$> (x Lude..@? "DataPath") Lude.<*> (x Lude..@? "Message")
