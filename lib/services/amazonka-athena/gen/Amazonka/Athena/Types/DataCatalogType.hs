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
-- Module      : Amazonka.Athena.Types.DataCatalogType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.DataCatalogType
  ( DataCatalogType
      ( ..,
        DataCatalogType_GLUE,
        DataCatalogType_HIVE,
        DataCatalogType_LAMBDA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataCatalogType = DataCatalogType'
  { fromDataCatalogType ::
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

pattern DataCatalogType_GLUE :: DataCatalogType
pattern DataCatalogType_GLUE = DataCatalogType' "GLUE"

pattern DataCatalogType_HIVE :: DataCatalogType
pattern DataCatalogType_HIVE = DataCatalogType' "HIVE"

pattern DataCatalogType_LAMBDA :: DataCatalogType
pattern DataCatalogType_LAMBDA = DataCatalogType' "LAMBDA"

{-# COMPLETE
  DataCatalogType_GLUE,
  DataCatalogType_HIVE,
  DataCatalogType_LAMBDA,
  DataCatalogType'
  #-}
