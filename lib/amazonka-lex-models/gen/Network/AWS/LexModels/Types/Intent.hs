{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Intent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.Intent
  ( Intent (..)
  -- * Smart constructor
  , mkIntent
  -- * Lenses
  , iIntentName
  , iIntentVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.IntentName as Types
import qualified Network.AWS.LexModels.Types.IntentVersion as Types
import qualified Network.AWS.Prelude as Core

-- | Identifies the specific version of an intent.
--
-- /See:/ 'mkIntent' smart constructor.
data Intent = Intent'
  { intentName :: Types.IntentName
    -- ^ The name of the intent.
  , intentVersion :: Types.IntentVersion
    -- ^ The version of the intent.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Intent' value with any optional fields omitted.
mkIntent
    :: Types.IntentName -- ^ 'intentName'
    -> Types.IntentVersion -- ^ 'intentVersion'
    -> Intent
mkIntent intentName intentVersion
  = Intent'{intentName, intentVersion}

-- | The name of the intent.
--
-- /Note:/ Consider using 'intentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iIntentName :: Lens.Lens' Intent Types.IntentName
iIntentName = Lens.field @"intentName"
{-# INLINEABLE iIntentName #-}
{-# DEPRECATED intentName "Use generic-lens or generic-optics with 'intentName' instead"  #-}

-- | The version of the intent.
--
-- /Note:/ Consider using 'intentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iIntentVersion :: Lens.Lens' Intent Types.IntentVersion
iIntentVersion = Lens.field @"intentVersion"
{-# INLINEABLE iIntentVersion #-}
{-# DEPRECATED intentVersion "Use generic-lens or generic-optics with 'intentVersion' instead"  #-}

instance Core.FromJSON Intent where
        toJSON Intent{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("intentName" Core..= intentName),
                  Core.Just ("intentVersion" Core..= intentVersion)])

instance Core.FromJSON Intent where
        parseJSON
          = Core.withObject "Intent" Core.$
              \ x ->
                Intent' Core.<$>
                  (x Core..: "intentName") Core.<*> x Core..: "intentVersion"
