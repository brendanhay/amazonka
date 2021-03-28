{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Column
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Column
  ( Column (..)
  -- * Smart constructor
  , mkColumn
  -- * Lenses
  , cName
  , cComment
  , cParameters
  , cType
  ) where

import qualified Network.AWS.Glue.Types.ColumnTypeString as Types
import qualified Network.AWS.Glue.Types.CommentString as Types
import qualified Network.AWS.Glue.Types.KeyString as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.ParametersMapValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A column in a @Table@ .
--
-- /See:/ 'mkColumn' smart constructor.
data Column = Column'
  { name :: Types.NameString
    -- ^ The name of the @Column@ .
  , comment :: Core.Maybe Types.CommentString
    -- ^ A free-form text comment.
  , parameters :: Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue)
    -- ^ These key-value pairs define properties associated with the column.
  , type' :: Core.Maybe Types.ColumnTypeString
    -- ^ The data type of the @Column@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Column' value with any optional fields omitted.
mkColumn
    :: Types.NameString -- ^ 'name'
    -> Column
mkColumn name
  = Column'{name, comment = Core.Nothing, parameters = Core.Nothing,
            type' = Core.Nothing}

-- | The name of the @Column@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Column Types.NameString
cName = Lens.field @"name"
{-# INLINEABLE cName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A free-form text comment.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cComment :: Lens.Lens' Column (Core.Maybe Types.CommentString)
cComment = Lens.field @"comment"
{-# INLINEABLE cComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

-- | These key-value pairs define properties associated with the column.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cParameters :: Lens.Lens' Column (Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue))
cParameters = Lens.field @"parameters"
{-# INLINEABLE cParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The data type of the @Column@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cType :: Lens.Lens' Column (Core.Maybe Types.ColumnTypeString)
cType = Lens.field @"type'"
{-# INLINEABLE cType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON Column where
        toJSON Column{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("Comment" Core..=) Core.<$> comment,
                  ("Parameters" Core..=) Core.<$> parameters,
                  ("Type" Core..=) Core.<$> type'])

instance Core.FromJSON Column where
        parseJSON
          = Core.withObject "Column" Core.$
              \ x ->
                Column' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..:? "Comment" Core.<*>
                    x Core..:? "Parameters"
                    Core.<*> x Core..:? "Type"
