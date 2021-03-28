{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchCreateObjectResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchCreateObjectResponse
  ( BatchCreateObjectResponse (..)
  -- * Smart constructor
  , mkBatchCreateObjectResponse
  -- * Lenses
  , bcorObjectIdentifier
  ) where

import qualified Network.AWS.CloudDirectory.Types.ObjectIdentifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'CreateObject' response operation.
--
-- /See:/ 'mkBatchCreateObjectResponse' smart constructor.
newtype BatchCreateObjectResponse = BatchCreateObjectResponse'
  { objectIdentifier :: Core.Maybe Types.ObjectIdentifier
    -- ^ The ID that is associated with the object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchCreateObjectResponse' value with any optional fields omitted.
mkBatchCreateObjectResponse
    :: BatchCreateObjectResponse
mkBatchCreateObjectResponse
  = BatchCreateObjectResponse'{objectIdentifier = Core.Nothing}

-- | The ID that is associated with the object.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcorObjectIdentifier :: Lens.Lens' BatchCreateObjectResponse (Core.Maybe Types.ObjectIdentifier)
bcorObjectIdentifier = Lens.field @"objectIdentifier"
{-# INLINEABLE bcorObjectIdentifier #-}
{-# DEPRECATED objectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead"  #-}

instance Core.FromJSON BatchCreateObjectResponse where
        parseJSON
          = Core.withObject "BatchCreateObjectResponse" Core.$
              \ x ->
                BatchCreateObjectResponse' Core.<$> (x Core..:? "ObjectIdentifier")
