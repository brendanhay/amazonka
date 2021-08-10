{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProductViewFilterBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProductViewFilterBy
  ( ProductViewFilterBy
      ( ..,
        ProductViewFilterBy_FullTextSearch,
        ProductViewFilterBy_Owner,
        ProductViewFilterBy_ProductType,
        ProductViewFilterBy_SourceProductId
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ProductViewFilterBy = ProductViewFilterBy'
  { fromProductViewFilterBy ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ProductViewFilterBy_FullTextSearch :: ProductViewFilterBy
pattern ProductViewFilterBy_FullTextSearch = ProductViewFilterBy' "FullTextSearch"

pattern ProductViewFilterBy_Owner :: ProductViewFilterBy
pattern ProductViewFilterBy_Owner = ProductViewFilterBy' "Owner"

pattern ProductViewFilterBy_ProductType :: ProductViewFilterBy
pattern ProductViewFilterBy_ProductType = ProductViewFilterBy' "ProductType"

pattern ProductViewFilterBy_SourceProductId :: ProductViewFilterBy
pattern ProductViewFilterBy_SourceProductId = ProductViewFilterBy' "SourceProductId"

{-# COMPLETE
  ProductViewFilterBy_FullTextSearch,
  ProductViewFilterBy_Owner,
  ProductViewFilterBy_ProductType,
  ProductViewFilterBy_SourceProductId,
  ProductViewFilterBy'
  #-}
