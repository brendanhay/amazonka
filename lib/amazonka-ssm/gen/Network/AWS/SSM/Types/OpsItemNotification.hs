{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.OpsItemNotification
  ( OpsItemNotification (..)
  -- * Smart constructor
  , mkOpsItemNotification
  -- * Lenses
  , oinArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A notification about the OpsItem.
--
-- /See:/ 'mkOpsItemNotification' smart constructor.
newtype OpsItemNotification = OpsItemNotification'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OpsItemNotification' value with any optional fields omitted.
mkOpsItemNotification
    :: OpsItemNotification
mkOpsItemNotification = OpsItemNotification'{arn = Core.Nothing}

-- | The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oinArn :: Lens.Lens' OpsItemNotification (Core.Maybe Core.Text)
oinArn = Lens.field @"arn"
{-# INLINEABLE oinArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.FromJSON OpsItemNotification where
        toJSON OpsItemNotification{..}
          = Core.object (Core.catMaybes [("Arn" Core..=) Core.<$> arn])

instance Core.FromJSON OpsItemNotification where
        parseJSON
          = Core.withObject "OpsItemNotification" Core.$
              \ x -> OpsItemNotification' Core.<$> (x Core..:? "Arn")
