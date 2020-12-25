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

import qualified Network.AWS.CloudWatch.Types.DataPath as Types
import qualified Network.AWS.CloudWatch.Types.Message as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An error or warning for the operation.
--
-- /See:/ 'mkDashboardValidationMessage' smart constructor.
data DashboardValidationMessage = DashboardValidationMessage'
  { -- | The data path related to the message.
    dataPath :: Core.Maybe Types.DataPath,
    -- | A message describing the error or warning.
    message :: Core.Maybe Types.Message
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DashboardValidationMessage' value with any optional fields omitted.
mkDashboardValidationMessage ::
  DashboardValidationMessage
mkDashboardValidationMessage =
  DashboardValidationMessage'
    { dataPath = Core.Nothing,
      message = Core.Nothing
    }

-- | The data path related to the message.
--
-- /Note:/ Consider using 'dataPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmDataPath :: Lens.Lens' DashboardValidationMessage (Core.Maybe Types.DataPath)
dvmDataPath = Lens.field @"dataPath"
{-# DEPRECATED dvmDataPath "Use generic-lens or generic-optics with 'dataPath' instead." #-}

-- | A message describing the error or warning.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmMessage :: Lens.Lens' DashboardValidationMessage (Core.Maybe Types.Message)
dvmMessage = Lens.field @"message"
{-# DEPRECATED dvmMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromXML DashboardValidationMessage where
  parseXML x =
    DashboardValidationMessage'
      Core.<$> (x Core..@? "DataPath") Core.<*> (x Core..@? "Message")
