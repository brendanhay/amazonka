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
-- Module      : Amazonka.ServiceCatalog.Types.ProductViewFilterBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ProductViewFilterBy
  ( ProductViewFilterBy
      ( ..,
        ProductViewFilterBy_FullTextSearch,
        ProductViewFilterBy_Owner,
        ProductViewFilterBy_ProductType,
        ProductViewFilterBy_SourceProductId
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProductViewFilterBy = ProductViewFilterBy'
  { fromProductViewFilterBy ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
