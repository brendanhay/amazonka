{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.TextArrayOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.TextArrayOptions
  ( TextArrayOptions (..),

    -- * Smart constructor
    mkTextArrayOptions,

    -- * Lenses
    taoAnalysisScheme,
    taoDefaultValue,
    taoHighlightEnabled,
    taoReturnEnabled,
    taoSourceFields,
  )
where

import qualified Network.AWS.CloudSearch.Types.DefaultValue as Types
import qualified Network.AWS.CloudSearch.Types.SourceFields as Types
import qualified Network.AWS.CloudSearch.Types.Word as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options for a field that contains an array of text strings. Present if @IndexFieldType@ specifies the field is of type @text-array@ . A @text-array@ field is always searchable. All options are enabled by default.
--
-- /See:/ 'mkTextArrayOptions' smart constructor.
data TextArrayOptions = TextArrayOptions'
  { -- | The name of an analysis scheme for a @text-array@ field.
    analysisScheme :: Core.Maybe Types.Word,
    -- | A value to use for the field if the field isn't specified for a document.
    defaultValue :: Core.Maybe Types.DefaultValue,
    -- | Whether highlights can be returned for the field.
    highlightEnabled :: Core.Maybe Core.Bool,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Core.Maybe Core.Bool,
    -- | A list of source fields to map to the field.
    sourceFields :: Core.Maybe Types.SourceFields
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TextArrayOptions' value with any optional fields omitted.
mkTextArrayOptions ::
  TextArrayOptions
mkTextArrayOptions =
  TextArrayOptions'
    { analysisScheme = Core.Nothing,
      defaultValue = Core.Nothing,
      highlightEnabled = Core.Nothing,
      returnEnabled = Core.Nothing,
      sourceFields = Core.Nothing
    }

-- | The name of an analysis scheme for a @text-array@ field.
--
-- /Note:/ Consider using 'analysisScheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taoAnalysisScheme :: Lens.Lens' TextArrayOptions (Core.Maybe Types.Word)
taoAnalysisScheme = Lens.field @"analysisScheme"
{-# DEPRECATED taoAnalysisScheme "Use generic-lens or generic-optics with 'analysisScheme' instead." #-}

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taoDefaultValue :: Lens.Lens' TextArrayOptions (Core.Maybe Types.DefaultValue)
taoDefaultValue = Lens.field @"defaultValue"
{-# DEPRECATED taoDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | Whether highlights can be returned for the field.
--
-- /Note:/ Consider using 'highlightEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taoHighlightEnabled :: Lens.Lens' TextArrayOptions (Core.Maybe Core.Bool)
taoHighlightEnabled = Lens.field @"highlightEnabled"
{-# DEPRECATED taoHighlightEnabled "Use generic-lens or generic-optics with 'highlightEnabled' instead." #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taoReturnEnabled :: Lens.Lens' TextArrayOptions (Core.Maybe Core.Bool)
taoReturnEnabled = Lens.field @"returnEnabled"
{-# DEPRECATED taoReturnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead." #-}

-- | A list of source fields to map to the field.
--
-- /Note:/ Consider using 'sourceFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taoSourceFields :: Lens.Lens' TextArrayOptions (Core.Maybe Types.SourceFields)
taoSourceFields = Lens.field @"sourceFields"
{-# DEPRECATED taoSourceFields "Use generic-lens or generic-optics with 'sourceFields' instead." #-}

instance Core.FromXML TextArrayOptions where
  parseXML x =
    TextArrayOptions'
      Core.<$> (x Core..@? "AnalysisScheme")
      Core.<*> (x Core..@? "DefaultValue")
      Core.<*> (x Core..@? "HighlightEnabled")
      Core.<*> (x Core..@? "ReturnEnabled")
      Core.<*> (x Core..@? "SourceFields")
