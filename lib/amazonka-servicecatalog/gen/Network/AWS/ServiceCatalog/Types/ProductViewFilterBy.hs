{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProductViewFilterBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProductViewFilterBy
  ( ProductViewFilterBy
    ( ProductViewFilterBy'
    , ProductViewFilterByFullTextSearch
    , ProductViewFilterByOwner
    , ProductViewFilterByProductType
    , ProductViewFilterBySourceProductId
    , fromProductViewFilterBy
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ProductViewFilterBy = ProductViewFilterBy'{fromProductViewFilterBy
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern ProductViewFilterByFullTextSearch :: ProductViewFilterBy
pattern ProductViewFilterByFullTextSearch = ProductViewFilterBy' "FullTextSearch"

pattern ProductViewFilterByOwner :: ProductViewFilterBy
pattern ProductViewFilterByOwner = ProductViewFilterBy' "Owner"

pattern ProductViewFilterByProductType :: ProductViewFilterBy
pattern ProductViewFilterByProductType = ProductViewFilterBy' "ProductType"

pattern ProductViewFilterBySourceProductId :: ProductViewFilterBy
pattern ProductViewFilterBySourceProductId = ProductViewFilterBy' "SourceProductId"

{-# COMPLETE 
  ProductViewFilterByFullTextSearch,

  ProductViewFilterByOwner,

  ProductViewFilterByProductType,

  ProductViewFilterBySourceProductId,
  ProductViewFilterBy'
  #-}
