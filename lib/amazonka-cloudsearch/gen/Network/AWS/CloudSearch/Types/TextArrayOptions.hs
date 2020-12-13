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
    taoSourceFields,
    taoReturnEnabled,
    taoAnalysisScheme,
    taoHighlightEnabled,
    taoDefaultValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Options for a field that contains an array of text strings. Present if @IndexFieldType@ specifies the field is of type @text-array@ . A @text-array@ field is always searchable. All options are enabled by default.
--
-- /See:/ 'mkTextArrayOptions' smart constructor.
data TextArrayOptions = TextArrayOptions'
  { -- | A list of source fields to map to the field.
    sourceFields :: Lude.Maybe Lude.Text,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Lude.Maybe Lude.Bool,
    -- | The name of an analysis scheme for a @text-array@ field.
    analysisScheme :: Lude.Maybe Lude.Text,
    -- | Whether highlights can be returned for the field.
    highlightEnabled :: Lude.Maybe Lude.Bool,
    -- | A value to use for the field if the field isn't specified for a document.
    defaultValue :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TextArrayOptions' with the minimum fields required to make a request.
--
-- * 'sourceFields' - A list of source fields to map to the field.
-- * 'returnEnabled' - Whether the contents of the field can be returned in the search results.
-- * 'analysisScheme' - The name of an analysis scheme for a @text-array@ field.
-- * 'highlightEnabled' - Whether highlights can be returned for the field.
-- * 'defaultValue' - A value to use for the field if the field isn't specified for a document.
mkTextArrayOptions ::
  TextArrayOptions
mkTextArrayOptions =
  TextArrayOptions'
    { sourceFields = Lude.Nothing,
      returnEnabled = Lude.Nothing,
      analysisScheme = Lude.Nothing,
      highlightEnabled = Lude.Nothing,
      defaultValue = Lude.Nothing
    }

-- | A list of source fields to map to the field.
--
-- /Note:/ Consider using 'sourceFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taoSourceFields :: Lens.Lens' TextArrayOptions (Lude.Maybe Lude.Text)
taoSourceFields = Lens.lens (sourceFields :: TextArrayOptions -> Lude.Maybe Lude.Text) (\s a -> s {sourceFields = a} :: TextArrayOptions)
{-# DEPRECATED taoSourceFields "Use generic-lens or generic-optics with 'sourceFields' instead." #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taoReturnEnabled :: Lens.Lens' TextArrayOptions (Lude.Maybe Lude.Bool)
taoReturnEnabled = Lens.lens (returnEnabled :: TextArrayOptions -> Lude.Maybe Lude.Bool) (\s a -> s {returnEnabled = a} :: TextArrayOptions)
{-# DEPRECATED taoReturnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead." #-}

-- | The name of an analysis scheme for a @text-array@ field.
--
-- /Note:/ Consider using 'analysisScheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taoAnalysisScheme :: Lens.Lens' TextArrayOptions (Lude.Maybe Lude.Text)
taoAnalysisScheme = Lens.lens (analysisScheme :: TextArrayOptions -> Lude.Maybe Lude.Text) (\s a -> s {analysisScheme = a} :: TextArrayOptions)
{-# DEPRECATED taoAnalysisScheme "Use generic-lens or generic-optics with 'analysisScheme' instead." #-}

-- | Whether highlights can be returned for the field.
--
-- /Note:/ Consider using 'highlightEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taoHighlightEnabled :: Lens.Lens' TextArrayOptions (Lude.Maybe Lude.Bool)
taoHighlightEnabled = Lens.lens (highlightEnabled :: TextArrayOptions -> Lude.Maybe Lude.Bool) (\s a -> s {highlightEnabled = a} :: TextArrayOptions)
{-# DEPRECATED taoHighlightEnabled "Use generic-lens or generic-optics with 'highlightEnabled' instead." #-}

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taoDefaultValue :: Lens.Lens' TextArrayOptions (Lude.Maybe Lude.Text)
taoDefaultValue = Lens.lens (defaultValue :: TextArrayOptions -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: TextArrayOptions)
{-# DEPRECATED taoDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

instance Lude.FromXML TextArrayOptions where
  parseXML x =
    TextArrayOptions'
      Lude.<$> (x Lude..@? "SourceFields")
      Lude.<*> (x Lude..@? "ReturnEnabled")
      Lude.<*> (x Lude..@? "AnalysisScheme")
      Lude.<*> (x Lude..@? "HighlightEnabled")
      Lude.<*> (x Lude..@? "DefaultValue")

instance Lude.ToQuery TextArrayOptions where
  toQuery TextArrayOptions' {..} =
    Lude.mconcat
      [ "SourceFields" Lude.=: sourceFields,
        "ReturnEnabled" Lude.=: returnEnabled,
        "AnalysisScheme" Lude.=: analysisScheme,
        "HighlightEnabled" Lude.=: highlightEnabled,
        "DefaultValue" Lude.=: defaultValue
      ]
