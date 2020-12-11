-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.Suggester
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.Suggester
  ( Suggester (..),

    -- * Smart constructor
    mkSuggester,

    -- * Lenses
    sSuggesterName,
    sDocumentSuggesterOptions,
  )
where

import Network.AWS.CloudSearch.Types.DocumentSuggesterOptions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration information for a search suggester. Each suggester has a unique name and specifies the text field you want to use for suggestions. The following options can be configured for a suggester: @FuzzyMatching@ , @SortExpression@ .
--
-- /See:/ 'mkSuggester' smart constructor.
data Suggester = Suggester'
  { suggesterName :: Lude.Text,
    documentSuggesterOptions :: DocumentSuggesterOptions
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Suggester' with the minimum fields required to make a request.
--
-- * 'documentSuggesterOptions' - Undocumented field.
-- * 'suggesterName' - Undocumented field.
mkSuggester ::
  -- | 'suggesterName'
  Lude.Text ->
  -- | 'documentSuggesterOptions'
  DocumentSuggesterOptions ->
  Suggester
mkSuggester pSuggesterName_ pDocumentSuggesterOptions_ =
  Suggester'
    { suggesterName = pSuggesterName_,
      documentSuggesterOptions = pDocumentSuggesterOptions_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'suggesterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSuggesterName :: Lens.Lens' Suggester Lude.Text
sSuggesterName = Lens.lens (suggesterName :: Suggester -> Lude.Text) (\s a -> s {suggesterName = a} :: Suggester)
{-# DEPRECATED sSuggesterName "Use generic-lens or generic-optics with 'suggesterName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'documentSuggesterOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDocumentSuggesterOptions :: Lens.Lens' Suggester DocumentSuggesterOptions
sDocumentSuggesterOptions = Lens.lens (documentSuggesterOptions :: Suggester -> DocumentSuggesterOptions) (\s a -> s {documentSuggesterOptions = a} :: Suggester)
{-# DEPRECATED sDocumentSuggesterOptions "Use generic-lens or generic-optics with 'documentSuggesterOptions' instead." #-}

instance Lude.FromXML Suggester where
  parseXML x =
    Suggester'
      Lude.<$> (x Lude..@ "SuggesterName")
      Lude.<*> (x Lude..@ "DocumentSuggesterOptions")

instance Lude.ToQuery Suggester where
  toQuery Suggester' {..} =
    Lude.mconcat
      [ "SuggesterName" Lude.=: suggesterName,
        "DocumentSuggesterOptions" Lude.=: documentSuggesterOptions
      ]
