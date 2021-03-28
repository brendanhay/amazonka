{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.DataSourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.DataSourceType
  ( DataSourceType
    ( DataSourceType'
    , DataSourceTypeAwsLambda
    , DataSourceTypeAmazonDynamodb
    , DataSourceTypeAmazonElasticsearch
    , DataSourceTypeNone
    , DataSourceTypeHttp
    , DataSourceTypeRelationalDatabase
    , fromDataSourceType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DataSourceType = DataSourceType'{fromDataSourceType ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern DataSourceTypeAwsLambda :: DataSourceType
pattern DataSourceTypeAwsLambda = DataSourceType' "AWS_LAMBDA"

pattern DataSourceTypeAmazonDynamodb :: DataSourceType
pattern DataSourceTypeAmazonDynamodb = DataSourceType' "AMAZON_DYNAMODB"

pattern DataSourceTypeAmazonElasticsearch :: DataSourceType
pattern DataSourceTypeAmazonElasticsearch = DataSourceType' "AMAZON_ELASTICSEARCH"

pattern DataSourceTypeNone :: DataSourceType
pattern DataSourceTypeNone = DataSourceType' "NONE"

pattern DataSourceTypeHttp :: DataSourceType
pattern DataSourceTypeHttp = DataSourceType' "HTTP"

pattern DataSourceTypeRelationalDatabase :: DataSourceType
pattern DataSourceTypeRelationalDatabase = DataSourceType' "RELATIONAL_DATABASE"

{-# COMPLETE 
  DataSourceTypeAwsLambda,

  DataSourceTypeAmazonDynamodb,

  DataSourceTypeAmazonElasticsearch,

  DataSourceTypeNone,

  DataSourceTypeHttp,

  DataSourceTypeRelationalDatabase,
  DataSourceType'
  #-}
