{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.TextOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.TextOptions
  ( TextOptions (..)
  -- * Smart constructor
  , mkTextOptions
  -- * Lenses
  , toAnalysisScheme
  , toDefaultValue
  , toHighlightEnabled
  , toReturnEnabled
  , toSortEnabled
  , toSourceField
  ) where

import qualified Network.AWS.CloudSearch.Types.DefaultValue as Types
import qualified Network.AWS.CloudSearch.Types.SourceField as Types
import qualified Network.AWS.CloudSearch.Types.Word as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options for text field. Present if @IndexFieldType@ specifies the field is of type @text@ . A @text@ field is always searchable. All options are enabled by default.
--
-- /See:/ 'mkTextOptions' smart constructor.
data TextOptions = TextOptions'
  { analysisScheme :: Core.Maybe Types.Word
    -- ^ The name of an analysis scheme for a @text@ field.
  , defaultValue :: Core.Maybe Types.DefaultValue
    -- ^ A value to use for the field if the field isn't specified for a document.
  , highlightEnabled :: Core.Maybe Core.Bool
    -- ^ Whether highlights can be returned for the field.
  , returnEnabled :: Core.Maybe Core.Bool
    -- ^ Whether the contents of the field can be returned in the search results.
  , sortEnabled :: Core.Maybe Core.Bool
    -- ^ Whether the field can be used to sort the search results.
  , sourceField :: Core.Maybe Types.SourceField
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TextOptions' value with any optional fields omitted.
mkTextOptions
    :: TextOptions
mkTextOptions
  = TextOptions'{analysisScheme = Core.Nothing,
                 defaultValue = Core.Nothing, highlightEnabled = Core.Nothing,
                 returnEnabled = Core.Nothing, sortEnabled = Core.Nothing,
                 sourceField = Core.Nothing}

-- | The name of an analysis scheme for a @text@ field.
--
-- /Note:/ Consider using 'analysisScheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toAnalysisScheme :: Lens.Lens' TextOptions (Core.Maybe Types.Word)
toAnalysisScheme = Lens.field @"analysisScheme"
{-# INLINEABLE toAnalysisScheme #-}
{-# DEPRECATED analysisScheme "Use generic-lens or generic-optics with 'analysisScheme' instead"  #-}

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toDefaultValue :: Lens.Lens' TextOptions (Core.Maybe Types.DefaultValue)
toDefaultValue = Lens.field @"defaultValue"
{-# INLINEABLE toDefaultValue #-}
{-# DEPRECATED defaultValue "Use generic-lens or generic-optics with 'defaultValue' instead"  #-}

-- | Whether highlights can be returned for the field.
--
-- /Note:/ Consider using 'highlightEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toHighlightEnabled :: Lens.Lens' TextOptions (Core.Maybe Core.Bool)
toHighlightEnabled = Lens.field @"highlightEnabled"
{-# INLINEABLE toHighlightEnabled #-}
{-# DEPRECATED highlightEnabled "Use generic-lens or generic-optics with 'highlightEnabled' instead"  #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toReturnEnabled :: Lens.Lens' TextOptions (Core.Maybe Core.Bool)
toReturnEnabled = Lens.field @"returnEnabled"
{-# INLINEABLE toReturnEnabled #-}
{-# DEPRECATED returnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead"  #-}

-- | Whether the field can be used to sort the search results.
--
-- /Note:/ Consider using 'sortEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toSortEnabled :: Lens.Lens' TextOptions (Core.Maybe Core.Bool)
toSortEnabled = Lens.field @"sortEnabled"
{-# INLINEABLE toSortEnabled #-}
{-# DEPRECATED sortEnabled "Use generic-lens or generic-optics with 'sortEnabled' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sourceField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toSourceField :: Lens.Lens' TextOptions (Core.Maybe Types.SourceField)
toSourceField = Lens.field @"sourceField"
{-# INLINEABLE toSourceField #-}
{-# DEPRECATED sourceField "Use generic-lens or generic-optics with 'sourceField' instead"  #-}

instance Core.ToQuery TextOptions where
        toQuery TextOptions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "AnalysisScheme")
              analysisScheme
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DefaultValue")
                defaultValue
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HighlightEnabled")
                highlightEnabled
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ReturnEnabled")
                returnEnabled
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SortEnabled") sortEnabled
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SourceField") sourceField

instance Core.FromXML TextOptions where
        parseXML x
          = TextOptions' Core.<$>
              (x Core..@? "AnalysisScheme") Core.<*> x Core..@? "DefaultValue"
                Core.<*> x Core..@? "HighlightEnabled"
                Core.<*> x Core..@? "ReturnEnabled"
                Core.<*> x Core..@? "SortEnabled"
                Core.<*> x Core..@? "SourceField"
