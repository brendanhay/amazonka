{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.TextMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.TextMessage
  ( TextMessage (..)
  -- * Smart constructor
  , mkTextMessage
  -- * Lenses
  , tmLocale
  , tmValue
  ) where

import qualified Network.AWS.AlexaBusiness.Types.Locale as Types
import qualified Network.AWS.AlexaBusiness.Types.TextValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The text message.
--
-- /See:/ 'mkTextMessage' smart constructor.
data TextMessage = TextMessage'
  { locale :: Types.Locale
    -- ^ The locale of the text message. Currently, en-US is supported.
  , value :: Types.TextValue
    -- ^ The value of the text message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TextMessage' value with any optional fields omitted.
mkTextMessage
    :: Types.Locale -- ^ 'locale'
    -> Types.TextValue -- ^ 'value'
    -> TextMessage
mkTextMessage locale value = TextMessage'{locale, value}

-- | The locale of the text message. Currently, en-US is supported.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmLocale :: Lens.Lens' TextMessage Types.Locale
tmLocale = Lens.field @"locale"
{-# INLINEABLE tmLocale #-}
{-# DEPRECATED locale "Use generic-lens or generic-optics with 'locale' instead"  #-}

-- | The value of the text message.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmValue :: Lens.Lens' TextMessage Types.TextValue
tmValue = Lens.field @"value"
{-# INLINEABLE tmValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON TextMessage where
        toJSON TextMessage{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Locale" Core..= locale),
                  Core.Just ("Value" Core..= value)])
