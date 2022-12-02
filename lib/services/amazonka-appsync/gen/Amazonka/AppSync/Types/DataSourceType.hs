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
-- Module      : Amazonka.AppSync.Types.DataSourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.DataSourceType
  ( DataSourceType
      ( ..,
        DataSourceType_AMAZON_DYNAMODB,
        DataSourceType_AMAZON_ELASTICSEARCH,
        DataSourceType_AMAZON_OPENSEARCH_SERVICE,
        DataSourceType_AWS_LAMBDA,
        DataSourceType_HTTP,
        DataSourceType_NONE,
        DataSourceType_RELATIONAL_DATABASE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataSourceType = DataSourceType'
  { fromDataSourceType ::
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

pattern DataSourceType_AMAZON_DYNAMODB :: DataSourceType
pattern DataSourceType_AMAZON_DYNAMODB = DataSourceType' "AMAZON_DYNAMODB"

pattern DataSourceType_AMAZON_ELASTICSEARCH :: DataSourceType
pattern DataSourceType_AMAZON_ELASTICSEARCH = DataSourceType' "AMAZON_ELASTICSEARCH"

pattern DataSourceType_AMAZON_OPENSEARCH_SERVICE :: DataSourceType
pattern DataSourceType_AMAZON_OPENSEARCH_SERVICE = DataSourceType' "AMAZON_OPENSEARCH_SERVICE"

pattern DataSourceType_AWS_LAMBDA :: DataSourceType
pattern DataSourceType_AWS_LAMBDA = DataSourceType' "AWS_LAMBDA"

pattern DataSourceType_HTTP :: DataSourceType
pattern DataSourceType_HTTP = DataSourceType' "HTTP"

pattern DataSourceType_NONE :: DataSourceType
pattern DataSourceType_NONE = DataSourceType' "NONE"

pattern DataSourceType_RELATIONAL_DATABASE :: DataSourceType
pattern DataSourceType_RELATIONAL_DATABASE = DataSourceType' "RELATIONAL_DATABASE"

{-# COMPLETE
  DataSourceType_AMAZON_DYNAMODB,
  DataSourceType_AMAZON_ELASTICSEARCH,
  DataSourceType_AMAZON_OPENSEARCH_SERVICE,
  DataSourceType_AWS_LAMBDA,
  DataSourceType_HTTP,
  DataSourceType_NONE,
  DataSourceType_RELATIONAL_DATABASE,
  DataSourceType'
  #-}
