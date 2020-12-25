{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.IPSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.IPSetSummary
  ( IPSetSummary (..),

    -- * Smart constructor
    mkIPSetSummary,

    -- * Lenses
    ipssIPSetId,
    ipssName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.IPSetId as Types
import qualified Network.AWS.WAFRegional.Types.Name as Types

-- | Contains the identifier and the name of the @IPSet@ .
--
-- /See:/ 'mkIPSetSummary' smart constructor.
data IPSetSummary = IPSetSummary'
  { -- | The @IPSetId@ for an 'IPSet' . You can use @IPSetId@ in a 'GetIPSet' request to get detailed information about an 'IPSet' .
    iPSetId :: Types.IPSetId,
    -- | A friendly name or description of the 'IPSet' . You can't change the name of an @IPSet@ after you create it.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IPSetSummary' value with any optional fields omitted.
mkIPSetSummary ::
  -- | 'iPSetId'
  Types.IPSetId ->
  -- | 'name'
  Types.Name ->
  IPSetSummary
mkIPSetSummary iPSetId name = IPSetSummary' {iPSetId, name}

-- | The @IPSetId@ for an 'IPSet' . You can use @IPSetId@ in a 'GetIPSet' request to get detailed information about an 'IPSet' .
--
-- /Note:/ Consider using 'iPSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipssIPSetId :: Lens.Lens' IPSetSummary Types.IPSetId
ipssIPSetId = Lens.field @"iPSetId"
{-# DEPRECATED ipssIPSetId "Use generic-lens or generic-optics with 'iPSetId' instead." #-}

-- | A friendly name or description of the 'IPSet' . You can't change the name of an @IPSet@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipssName :: Lens.Lens' IPSetSummary Types.Name
ipssName = Lens.field @"name"
{-# DEPRECATED ipssName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON IPSetSummary where
  parseJSON =
    Core.withObject "IPSetSummary" Core.$
      \x ->
        IPSetSummary'
          Core.<$> (x Core..: "IPSetId") Core.<*> (x Core..: "Name")
