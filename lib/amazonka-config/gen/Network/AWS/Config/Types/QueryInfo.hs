{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.QueryInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.QueryInfo
  ( QueryInfo (..)
  -- * Smart constructor
  , mkQueryInfo
  -- * Lenses
  , qiSelectFields
  ) where

import qualified Network.AWS.Config.Types.FieldInfo as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about the query.
--
-- /See:/ 'mkQueryInfo' smart constructor.
newtype QueryInfo = QueryInfo'
  { selectFields :: Core.Maybe [Types.FieldInfo]
    -- ^ Returns a @FieldInfo@ object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'QueryInfo' value with any optional fields omitted.
mkQueryInfo
    :: QueryInfo
mkQueryInfo = QueryInfo'{selectFields = Core.Nothing}

-- | Returns a @FieldInfo@ object.
--
-- /Note:/ Consider using 'selectFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qiSelectFields :: Lens.Lens' QueryInfo (Core.Maybe [Types.FieldInfo])
qiSelectFields = Lens.field @"selectFields"
{-# INLINEABLE qiSelectFields #-}
{-# DEPRECATED selectFields "Use generic-lens or generic-optics with 'selectFields' instead"  #-}

instance Core.FromJSON QueryInfo where
        parseJSON
          = Core.withObject "QueryInfo" Core.$
              \ x -> QueryInfo' Core.<$> (x Core..:? "SelectFields")
