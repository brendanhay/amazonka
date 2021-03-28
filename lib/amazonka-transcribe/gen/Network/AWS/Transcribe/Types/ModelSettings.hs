{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.ModelSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.ModelSettings
  ( ModelSettings (..)
  -- * Smart constructor
  , mkModelSettings
  -- * Lenses
  , msLanguageModelName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Transcribe.Types.ModelName as Types

-- | The object used to call your custom language model to your transcription job.
--
-- /See:/ 'mkModelSettings' smart constructor.
newtype ModelSettings = ModelSettings'
  { languageModelName :: Core.Maybe Types.ModelName
    -- ^ The name of your custom language model.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ModelSettings' value with any optional fields omitted.
mkModelSettings
    :: ModelSettings
mkModelSettings = ModelSettings'{languageModelName = Core.Nothing}

-- | The name of your custom language model.
--
-- /Note:/ Consider using 'languageModelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msLanguageModelName :: Lens.Lens' ModelSettings (Core.Maybe Types.ModelName)
msLanguageModelName = Lens.field @"languageModelName"
{-# INLINEABLE msLanguageModelName #-}
{-# DEPRECATED languageModelName "Use generic-lens or generic-optics with 'languageModelName' instead"  #-}

instance Core.FromJSON ModelSettings where
        toJSON ModelSettings{..}
          = Core.object
              (Core.catMaybes
                 [("LanguageModelName" Core..=) Core.<$> languageModelName])

instance Core.FromJSON ModelSettings where
        parseJSON
          = Core.withObject "ModelSettings" Core.$
              \ x -> ModelSettings' Core.<$> (x Core..:? "LanguageModelName")
