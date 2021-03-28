{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.Suggester
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.Suggester
  ( Suggester (..)
  -- * Smart constructor
  , mkSuggester
  -- * Lenses
  , sSuggesterName
  , sDocumentSuggesterOptions
  ) where

import qualified Network.AWS.CloudSearch.Types.DocumentSuggesterOptions as Types
import qualified Network.AWS.CloudSearch.Types.StandardName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration information for a search suggester. Each suggester has a unique name and specifies the text field you want to use for suggestions. The following options can be configured for a suggester: @FuzzyMatching@ , @SortExpression@ . 
--
-- /See:/ 'mkSuggester' smart constructor.
data Suggester = Suggester'
  { suggesterName :: Types.StandardName
  , documentSuggesterOptions :: Types.DocumentSuggesterOptions
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Suggester' value with any optional fields omitted.
mkSuggester
    :: Types.StandardName -- ^ 'suggesterName'
    -> Types.DocumentSuggesterOptions -- ^ 'documentSuggesterOptions'
    -> Suggester
mkSuggester suggesterName documentSuggesterOptions
  = Suggester'{suggesterName, documentSuggesterOptions}

-- | Undocumented field.
--
-- /Note:/ Consider using 'suggesterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSuggesterName :: Lens.Lens' Suggester Types.StandardName
sSuggesterName = Lens.field @"suggesterName"
{-# INLINEABLE sSuggesterName #-}
{-# DEPRECATED suggesterName "Use generic-lens or generic-optics with 'suggesterName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'documentSuggesterOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDocumentSuggesterOptions :: Lens.Lens' Suggester Types.DocumentSuggesterOptions
sDocumentSuggesterOptions = Lens.field @"documentSuggesterOptions"
{-# INLINEABLE sDocumentSuggesterOptions #-}
{-# DEPRECATED documentSuggesterOptions "Use generic-lens or generic-optics with 'documentSuggesterOptions' instead"  #-}

instance Core.ToQuery Suggester where
        toQuery Suggester{..}
          = Core.toQueryPair "SuggesterName" suggesterName Core.<>
              Core.toQueryPair "DocumentSuggesterOptions"
                documentSuggesterOptions

instance Core.FromXML Suggester where
        parseXML x
          = Suggester' Core.<$>
              (x Core..@ "SuggesterName") Core.<*>
                x Core..@ "DocumentSuggesterOptions"
