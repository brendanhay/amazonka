{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.RecordError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.RecordError
  ( RecordError (..)
  -- * Smart constructor
  , mkRecordError
  -- * Lenses
  , reCode
  , reDescription
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.Code as Types
import qualified Network.AWS.ServiceCatalog.Types.Description as Types

-- | The error code and description resulting from an operation.
--
-- /See:/ 'mkRecordError' smart constructor.
data RecordError = RecordError'
  { code :: Core.Maybe Types.Code
    -- ^ The numeric value of the error.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RecordError' value with any optional fields omitted.
mkRecordError
    :: RecordError
mkRecordError
  = RecordError'{code = Core.Nothing, description = Core.Nothing}

-- | The numeric value of the error.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reCode :: Lens.Lens' RecordError (Core.Maybe Types.Code)
reCode = Lens.field @"code"
{-# INLINEABLE reCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The description of the error.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reDescription :: Lens.Lens' RecordError (Core.Maybe Types.Description)
reDescription = Lens.field @"description"
{-# INLINEABLE reDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.FromJSON RecordError where
        parseJSON
          = Core.withObject "RecordError" Core.$
              \ x ->
                RecordError' Core.<$>
                  (x Core..:? "Code") Core.<*> x Core..:? "Description"
