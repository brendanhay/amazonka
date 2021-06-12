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
-- Module      : Network.AWS.AppSync.Types.DataSourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.DataSourceType
  ( DataSourceType
      ( ..,
        DataSourceType_AMAZON_DYNAMODB,
        DataSourceType_AMAZON_ELASTICSEARCH,
        DataSourceType_AWS_LAMBDA,
        DataSourceType_HTTP,
        DataSourceType_NONE,
        DataSourceType_RELATIONAL_DATABASE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype DataSourceType = DataSourceType'
  { fromDataSourceType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern DataSourceType_AMAZON_DYNAMODB :: DataSourceType
pattern DataSourceType_AMAZON_DYNAMODB = DataSourceType' "AMAZON_DYNAMODB"

pattern DataSourceType_AMAZON_ELASTICSEARCH :: DataSourceType
pattern DataSourceType_AMAZON_ELASTICSEARCH = DataSourceType' "AMAZON_ELASTICSEARCH"

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
  DataSourceType_AWS_LAMBDA,
  DataSourceType_HTTP,
  DataSourceType_NONE,
  DataSourceType_RELATIONAL_DATABASE,
  DataSourceType'
  #-}
