{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.AppliedTerminology
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.AppliedTerminology
  ( AppliedTerminology (..),

    -- * Smart constructor
    mkAppliedTerminology,

    -- * Lenses
    atName,
    atTerms,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Translate.Types.ResourceName as Types
import qualified Network.AWS.Translate.Types.Term as Types

-- | The custom terminology applied to the input text by Amazon Translate for the translated text response. This is optional in the response and will only be present if you specified terminology input in the request. Currently, only one terminology can be applied per TranslateText request.
--
-- /See:/ 'mkAppliedTerminology' smart constructor.
data AppliedTerminology = AppliedTerminology'
  { -- | The name of the custom terminology applied to the input text by Amazon Translate for the translated text response.
    name :: Core.Maybe Types.ResourceName,
    -- | The specific terms of the custom terminology applied to the input text by Amazon Translate for the translated text response. A maximum of 250 terms will be returned, and the specific terms applied will be the first 250 terms in the source text.
    terms :: Core.Maybe [Types.Term]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AppliedTerminology' value with any optional fields omitted.
mkAppliedTerminology ::
  AppliedTerminology
mkAppliedTerminology =
  AppliedTerminology' {name = Core.Nothing, terms = Core.Nothing}

-- | The name of the custom terminology applied to the input text by Amazon Translate for the translated text response.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atName :: Lens.Lens' AppliedTerminology (Core.Maybe Types.ResourceName)
atName = Lens.field @"name"
{-# DEPRECATED atName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The specific terms of the custom terminology applied to the input text by Amazon Translate for the translated text response. A maximum of 250 terms will be returned, and the specific terms applied will be the first 250 terms in the source text.
--
-- /Note:/ Consider using 'terms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atTerms :: Lens.Lens' AppliedTerminology (Core.Maybe [Types.Term])
atTerms = Lens.field @"terms"
{-# DEPRECATED atTerms "Use generic-lens or generic-optics with 'terms' instead." #-}

instance Core.FromJSON AppliedTerminology where
  parseJSON =
    Core.withObject "AppliedTerminology" Core.$
      \x ->
        AppliedTerminology'
          Core.<$> (x Core..:? "Name") Core.<*> (x Core..:? "Terms")
