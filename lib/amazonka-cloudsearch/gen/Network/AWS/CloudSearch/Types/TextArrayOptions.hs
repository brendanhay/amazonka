{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.TextArrayOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.TextArrayOptions
  ( TextArrayOptions (..)
  -- * Smart constructor
  , mkTextArrayOptions
  -- * Lenses
  , taoAnalysisScheme
  , taoDefaultValue
  , taoHighlightEnabled
  , taoReturnEnabled
  , taoSourceFields
  ) where

import qualified Network.AWS.CloudSearch.Types.DefaultValue as Types
import qualified Network.AWS.CloudSearch.Types.SourceFields as Types
import qualified Network.AWS.CloudSearch.Types.Word as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options for a field that contains an array of text strings. Present if @IndexFieldType@ specifies the field is of type @text-array@ . A @text-array@ field is always searchable. All options are enabled by default.
--
-- /See:/ 'mkTextArrayOptions' smart constructor.
data TextArrayOptions = TextArrayOptions'
  { analysisScheme :: Core.Maybe Types.Word
    -- ^ The name of an analysis scheme for a @text-array@ field.
  , defaultValue :: Core.Maybe Types.DefaultValue
    -- ^ A value to use for the field if the field isn't specified for a document.
  , highlightEnabled :: Core.Maybe Core.Bool
    -- ^ Whether highlights can be returned for the field.
  , returnEnabled :: Core.Maybe Core.Bool
    -- ^ Whether the contents of the field can be returned in the search results.
  , sourceFields :: Core.Maybe Types.SourceFields
    -- ^ A list of source fields to map to the field. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TextArrayOptions' value with any optional fields omitted.
mkTextArrayOptions
    :: TextArrayOptions
mkTextArrayOptions
  = TextArrayOptions'{analysisScheme = Core.Nothing,
                      defaultValue = Core.Nothing, highlightEnabled = Core.Nothing,
                      returnEnabled = Core.Nothing, sourceFields = Core.Nothing}

-- | The name of an analysis scheme for a @text-array@ field.
--
-- /Note:/ Consider using 'analysisScheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taoAnalysisScheme :: Lens.Lens' TextArrayOptions (Core.Maybe Types.Word)
taoAnalysisScheme = Lens.field @"analysisScheme"
{-# INLINEABLE taoAnalysisScheme #-}
{-# DEPRECATED analysisScheme "Use generic-lens or generic-optics with 'analysisScheme' instead"  #-}

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taoDefaultValue :: Lens.Lens' TextArrayOptions (Core.Maybe Types.DefaultValue)
taoDefaultValue = Lens.field @"defaultValue"
{-# INLINEABLE taoDefaultValue #-}
{-# DEPRECATED defaultValue "Use generic-lens or generic-optics with 'defaultValue' instead"  #-}

-- | Whether highlights can be returned for the field.
--
-- /Note:/ Consider using 'highlightEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taoHighlightEnabled :: Lens.Lens' TextArrayOptions (Core.Maybe Core.Bool)
taoHighlightEnabled = Lens.field @"highlightEnabled"
{-# INLINEABLE taoHighlightEnabled #-}
{-# DEPRECATED highlightEnabled "Use generic-lens or generic-optics with 'highlightEnabled' instead"  #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taoReturnEnabled :: Lens.Lens' TextArrayOptions (Core.Maybe Core.Bool)
taoReturnEnabled = Lens.field @"returnEnabled"
{-# INLINEABLE taoReturnEnabled #-}
{-# DEPRECATED returnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead"  #-}

-- | A list of source fields to map to the field. 
--
-- /Note:/ Consider using 'sourceFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taoSourceFields :: Lens.Lens' TextArrayOptions (Core.Maybe Types.SourceFields)
taoSourceFields = Lens.field @"sourceFields"
{-# INLINEABLE taoSourceFields #-}
{-# DEPRECATED sourceFields "Use generic-lens or generic-optics with 'sourceFields' instead"  #-}

instance Core.ToQuery TextArrayOptions where
        toQuery TextArrayOptions{..}
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
              Core.maybe Core.mempty (Core.toQueryPair "SourceFields")
                sourceFields

instance Core.FromXML TextArrayOptions where
        parseXML x
          = TextArrayOptions' Core.<$>
              (x Core..@? "AnalysisScheme") Core.<*> x Core..@? "DefaultValue"
                Core.<*> x Core..@? "HighlightEnabled"
                Core.<*> x Core..@? "ReturnEnabled"
                Core.<*> x Core..@? "SourceFields"
