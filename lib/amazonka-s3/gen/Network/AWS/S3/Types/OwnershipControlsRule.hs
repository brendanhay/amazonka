{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.OwnershipControlsRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.OwnershipControlsRule
  ( OwnershipControlsRule (..)
  -- * Smart constructor
  , mkOwnershipControlsRule
  -- * Lenses
  , ocrObjectOwnership
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.ObjectOwnership as Types

-- | The container element for an ownership control rule.
--
-- /See:/ 'mkOwnershipControlsRule' smart constructor.
newtype OwnershipControlsRule = OwnershipControlsRule'
  { objectOwnership :: Types.ObjectOwnership
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OwnershipControlsRule' value with any optional fields omitted.
mkOwnershipControlsRule
    :: Types.ObjectOwnership -- ^ 'objectOwnership'
    -> OwnershipControlsRule
mkOwnershipControlsRule objectOwnership
  = OwnershipControlsRule'{objectOwnership}

-- | Undocumented field.
--
-- /Note:/ Consider using 'objectOwnership' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrObjectOwnership :: Lens.Lens' OwnershipControlsRule Types.ObjectOwnership
ocrObjectOwnership = Lens.field @"objectOwnership"
{-# INLINEABLE ocrObjectOwnership #-}
{-# DEPRECATED objectOwnership "Use generic-lens or generic-optics with 'objectOwnership' instead"  #-}

instance Core.ToXML OwnershipControlsRule where
        toXML OwnershipControlsRule{..}
          = Core.toXMLElement "ObjectOwnership" objectOwnership

instance Core.FromXML OwnershipControlsRule where
        parseXML x
          = OwnershipControlsRule' Core.<$> (x Core..@ "ObjectOwnership")
