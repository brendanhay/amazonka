{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SupportedOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.SupportedOperation
  ( SupportedOperation (..)
  -- * Smart constructor
  , mkSupportedOperation
  -- * Lenses
  , soOperationName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types

-- | Describes the operations that are allowed on a maintenance track.
--
-- /See:/ 'mkSupportedOperation' smart constructor.
newtype SupportedOperation = SupportedOperation'
  { operationName :: Core.Maybe Core.Text
    -- ^ A list of the supported operations.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SupportedOperation' value with any optional fields omitted.
mkSupportedOperation
    :: SupportedOperation
mkSupportedOperation
  = SupportedOperation'{operationName = Core.Nothing}

-- | A list of the supported operations.
--
-- /Note:/ Consider using 'operationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soOperationName :: Lens.Lens' SupportedOperation (Core.Maybe Core.Text)
soOperationName = Lens.field @"operationName"
{-# INLINEABLE soOperationName #-}
{-# DEPRECATED operationName "Use generic-lens or generic-optics with 'operationName' instead"  #-}

instance Core.FromXML SupportedOperation where
        parseXML x
          = SupportedOperation' Core.<$> (x Core..@? "OperationName")
