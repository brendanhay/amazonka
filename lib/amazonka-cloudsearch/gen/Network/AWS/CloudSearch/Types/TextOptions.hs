{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.TextOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.TextOptions
  ( TextOptions (..),

    -- * Smart constructor
    mkTextOptions,

    -- * Lenses
    toSourceField,
    toReturnEnabled,
    toAnalysisScheme,
    toHighlightEnabled,
    toSortEnabled,
    toDefaultValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Options for text field. Present if @IndexFieldType@ specifies the field is of type @text@ . A @text@ field is always searchable. All options are enabled by default.
--
-- /See:/ 'mkTextOptions' smart constructor.
data TextOptions = TextOptions'
  { sourceField ::
      Lude.Maybe Lude.Text,
    returnEnabled :: Lude.Maybe Lude.Bool,
    analysisScheme :: Lude.Maybe Lude.Text,
    highlightEnabled :: Lude.Maybe Lude.Bool,
    sortEnabled :: Lude.Maybe Lude.Bool,
    defaultValue :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TextOptions' with the minimum fields required to make a request.
--
-- * 'analysisScheme' - The name of an analysis scheme for a @text@ field.
-- * 'defaultValue' - A value to use for the field if the field isn't specified for a document.
-- * 'highlightEnabled' - Whether highlights can be returned for the field.
-- * 'returnEnabled' - Whether the contents of the field can be returned in the search results.
-- * 'sortEnabled' - Whether the field can be used to sort the search results.
-- * 'sourceField' - Undocumented field.
mkTextOptions ::
  TextOptions
mkTextOptions =
  TextOptions'
    { sourceField = Lude.Nothing,
      returnEnabled = Lude.Nothing,
      analysisScheme = Lude.Nothing,
      highlightEnabled = Lude.Nothing,
      sortEnabled = Lude.Nothing,
      defaultValue = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'sourceField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toSourceField :: Lens.Lens' TextOptions (Lude.Maybe Lude.Text)
toSourceField = Lens.lens (sourceField :: TextOptions -> Lude.Maybe Lude.Text) (\s a -> s {sourceField = a} :: TextOptions)
{-# DEPRECATED toSourceField "Use generic-lens or generic-optics with 'sourceField' instead." #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toReturnEnabled :: Lens.Lens' TextOptions (Lude.Maybe Lude.Bool)
toReturnEnabled = Lens.lens (returnEnabled :: TextOptions -> Lude.Maybe Lude.Bool) (\s a -> s {returnEnabled = a} :: TextOptions)
{-# DEPRECATED toReturnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead." #-}

-- | The name of an analysis scheme for a @text@ field.
--
-- /Note:/ Consider using 'analysisScheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toAnalysisScheme :: Lens.Lens' TextOptions (Lude.Maybe Lude.Text)
toAnalysisScheme = Lens.lens (analysisScheme :: TextOptions -> Lude.Maybe Lude.Text) (\s a -> s {analysisScheme = a} :: TextOptions)
{-# DEPRECATED toAnalysisScheme "Use generic-lens or generic-optics with 'analysisScheme' instead." #-}

-- | Whether highlights can be returned for the field.
--
-- /Note:/ Consider using 'highlightEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toHighlightEnabled :: Lens.Lens' TextOptions (Lude.Maybe Lude.Bool)
toHighlightEnabled = Lens.lens (highlightEnabled :: TextOptions -> Lude.Maybe Lude.Bool) (\s a -> s {highlightEnabled = a} :: TextOptions)
{-# DEPRECATED toHighlightEnabled "Use generic-lens or generic-optics with 'highlightEnabled' instead." #-}

-- | Whether the field can be used to sort the search results.
--
-- /Note:/ Consider using 'sortEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toSortEnabled :: Lens.Lens' TextOptions (Lude.Maybe Lude.Bool)
toSortEnabled = Lens.lens (sortEnabled :: TextOptions -> Lude.Maybe Lude.Bool) (\s a -> s {sortEnabled = a} :: TextOptions)
{-# DEPRECATED toSortEnabled "Use generic-lens or generic-optics with 'sortEnabled' instead." #-}

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toDefaultValue :: Lens.Lens' TextOptions (Lude.Maybe Lude.Text)
toDefaultValue = Lens.lens (defaultValue :: TextOptions -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: TextOptions)
{-# DEPRECATED toDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

instance Lude.FromXML TextOptions where
  parseXML x =
    TextOptions'
      Lude.<$> (x Lude..@? "SourceField")
      Lude.<*> (x Lude..@? "ReturnEnabled")
      Lude.<*> (x Lude..@? "AnalysisScheme")
      Lude.<*> (x Lude..@? "HighlightEnabled")
      Lude.<*> (x Lude..@? "SortEnabled")
      Lude.<*> (x Lude..@? "DefaultValue")

instance Lude.ToQuery TextOptions where
  toQuery TextOptions' {..} =
    Lude.mconcat
      [ "SourceField" Lude.=: sourceField,
        "ReturnEnabled" Lude.=: returnEnabled,
        "AnalysisScheme" Lude.=: analysisScheme,
        "HighlightEnabled" Lude.=: highlightEnabled,
        "SortEnabled" Lude.=: sortEnabled,
        "DefaultValue" Lude.=: defaultValue
      ]
