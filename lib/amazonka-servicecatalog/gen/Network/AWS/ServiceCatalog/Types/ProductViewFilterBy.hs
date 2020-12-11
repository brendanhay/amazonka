-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProductViewFilterBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProductViewFilterBy
  ( ProductViewFilterBy
      ( ProductViewFilterBy',
        PVFBFullTextSearch,
        PVFBOwner,
        PVFBProductType,
        PVFBSourceProductId
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ProductViewFilterBy = ProductViewFilterBy' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern PVFBFullTextSearch :: ProductViewFilterBy
pattern PVFBFullTextSearch = ProductViewFilterBy' "FullTextSearch"

pattern PVFBOwner :: ProductViewFilterBy
pattern PVFBOwner = ProductViewFilterBy' "Owner"

pattern PVFBProductType :: ProductViewFilterBy
pattern PVFBProductType = ProductViewFilterBy' "ProductType"

pattern PVFBSourceProductId :: ProductViewFilterBy
pattern PVFBSourceProductId = ProductViewFilterBy' "SourceProductId"

{-# COMPLETE
  PVFBFullTextSearch,
  PVFBOwner,
  PVFBProductType,
  PVFBSourceProductId,
  ProductViewFilterBy'
  #-}
