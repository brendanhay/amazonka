{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype DataSourceType = DataSourceType'
  { fromDataSourceType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
