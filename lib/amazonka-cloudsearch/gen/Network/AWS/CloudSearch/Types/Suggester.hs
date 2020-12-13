{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    sDocumentSuggesterOptions,
    sSuggesterName,
  )
where

import Network.AWS.CloudSearch.Types.DocumentSuggesterOptions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration information for a search suggester. Each suggester has a unique name and specifies the text field you want to use for suggestions. The following options can be configured for a suggester: @FuzzyMatching@ , @SortExpression@ .
--
-- /See:/ 'mkSuggester' smart constructor.
data Suggester = Suggester'
  { documentSuggesterOptions :: DocumentSuggesterOptions,
    suggesterName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Suggester' with the minimum fields required to make a request.
--
-- * 'documentSuggesterOptions' -
-- * 'suggesterName' -
mkSuggester ::
  -- | 'documentSuggesterOptions'
  DocumentSuggesterOptions ->
  -- | 'suggesterName'
  Lude.Text ->
  Suggester
mkSuggester pDocumentSuggesterOptions_ pSuggesterName_ =
  Suggester'
    { documentSuggesterOptions = pDocumentSuggesterOptions_,
      suggesterName = pSuggesterName_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'documentSuggesterOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDocumentSuggesterOptions :: Lens.Lens' Suggester DocumentSuggesterOptions
sDocumentSuggesterOptions = Lens.lens (documentSuggesterOptions :: Suggester -> DocumentSuggesterOptions) (\s a -> s {documentSuggesterOptions = a} :: Suggester)
{-# DEPRECATED sDocumentSuggesterOptions "Use generic-lens or generic-optics with 'documentSuggesterOptions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'suggesterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSuggesterName :: Lens.Lens' Suggester Lude.Text
sSuggesterName = Lens.lens (suggesterName :: Suggester -> Lude.Text) (\s a -> s {suggesterName = a} :: Suggester)
{-# DEPRECATED sSuggesterName "Use generic-lens or generic-optics with 'suggesterName' instead." #-}

instance Lude.FromXML Suggester where
  parseXML x =
    Suggester'
      Lude.<$> (x Lude..@ "DocumentSuggesterOptions")
      Lude.<*> (x Lude..@ "SuggesterName")

instance Lude.ToQuery Suggester where
  toQuery Suggester' {..} =
    Lude.mconcat
      [ "DocumentSuggesterOptions" Lude.=: documentSuggesterOptions,
        "SuggesterName" Lude.=: suggesterName
      ]
