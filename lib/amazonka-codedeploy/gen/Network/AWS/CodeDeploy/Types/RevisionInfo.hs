{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.RevisionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.RevisionInfo
  ( RevisionInfo (..)
  -- * Smart constructor
  , mkRevisionInfo
  -- * Lenses
  , riGenericRevisionInfo
  , riRevisionLocation
  ) where

import qualified Network.AWS.CodeDeploy.Types.GenericRevisionInfo as Types
import qualified Network.AWS.CodeDeploy.Types.RevisionLocation as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an application revision.
--
-- /See:/ 'mkRevisionInfo' smart constructor.
data RevisionInfo = RevisionInfo'
  { genericRevisionInfo :: Core.Maybe Types.GenericRevisionInfo
    -- ^ Information about an application revision, including usage details and associated deployment groups.
  , revisionLocation :: Core.Maybe Types.RevisionLocation
    -- ^ Information about the location and type of an application revision.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RevisionInfo' value with any optional fields omitted.
mkRevisionInfo
    :: RevisionInfo
mkRevisionInfo
  = RevisionInfo'{genericRevisionInfo = Core.Nothing,
                  revisionLocation = Core.Nothing}

-- | Information about an application revision, including usage details and associated deployment groups.
--
-- /Note:/ Consider using 'genericRevisionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riGenericRevisionInfo :: Lens.Lens' RevisionInfo (Core.Maybe Types.GenericRevisionInfo)
riGenericRevisionInfo = Lens.field @"genericRevisionInfo"
{-# INLINEABLE riGenericRevisionInfo #-}
{-# DEPRECATED genericRevisionInfo "Use generic-lens or generic-optics with 'genericRevisionInfo' instead"  #-}

-- | Information about the location and type of an application revision.
--
-- /Note:/ Consider using 'revisionLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRevisionLocation :: Lens.Lens' RevisionInfo (Core.Maybe Types.RevisionLocation)
riRevisionLocation = Lens.field @"revisionLocation"
{-# INLINEABLE riRevisionLocation #-}
{-# DEPRECATED revisionLocation "Use generic-lens or generic-optics with 'revisionLocation' instead"  #-}

instance Core.FromJSON RevisionInfo where
        parseJSON
          = Core.withObject "RevisionInfo" Core.$
              \ x ->
                RevisionInfo' Core.<$>
                  (x Core..:? "genericRevisionInfo") Core.<*>
                    x Core..:? "revisionLocation"
