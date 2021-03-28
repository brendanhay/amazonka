{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.MessageTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.MessageTag
  ( MessageTag (..)
  -- * Smart constructor
  , mkMessageTag
  -- * Lenses
  , mtName
  , mtValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.MessageTagName as Types
import qualified Network.AWS.SES.Types.Value as Types

-- | Contains the name and value of a tag that you can provide to @SendEmail@ or @SendRawEmail@ to apply to an email.
--
-- Message tags, which you use with configuration sets, enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkMessageTag' smart constructor.
data MessageTag = MessageTag'
  { name :: Types.MessageTagName
    -- ^ The name of the tag. The name must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Contain less than 256 characters.
--
--
  , value :: Types.Value
    -- ^ The value of the tag. The value must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Contain less than 256 characters.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MessageTag' value with any optional fields omitted.
mkMessageTag
    :: Types.MessageTagName -- ^ 'name'
    -> Types.Value -- ^ 'value'
    -> MessageTag
mkMessageTag name value = MessageTag'{name, value}

-- | The name of the tag. The name must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Contain less than 256 characters.
--
--
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtName :: Lens.Lens' MessageTag Types.MessageTagName
mtName = Lens.field @"name"
{-# INLINEABLE mtName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value of the tag. The value must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Contain less than 256 characters.
--
--
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtValue :: Lens.Lens' MessageTag Types.Value
mtValue = Lens.field @"value"
{-# INLINEABLE mtValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.ToQuery MessageTag where
        toQuery MessageTag{..}
          = Core.toQueryPair "Name" name Core.<>
              Core.toQueryPair "Value" value
