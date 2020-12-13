{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.DataSourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.DataSourceType
  ( DataSourceType
      ( DataSourceType',
        DSTAWSLambda,
        DSTAmazonDynamodb,
        DSTAmazonElasticsearch,
        DSTNone,
        DSTHTTP,
        DSTRelationalDatabase
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DataSourceType = DataSourceType' Lude.Text
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

pattern DSTAWSLambda :: DataSourceType
pattern DSTAWSLambda = DataSourceType' "AWS_LAMBDA"

pattern DSTAmazonDynamodb :: DataSourceType
pattern DSTAmazonDynamodb = DataSourceType' "AMAZON_DYNAMODB"

pattern DSTAmazonElasticsearch :: DataSourceType
pattern DSTAmazonElasticsearch = DataSourceType' "AMAZON_ELASTICSEARCH"

pattern DSTNone :: DataSourceType
pattern DSTNone = DataSourceType' "NONE"

pattern DSTHTTP :: DataSourceType
pattern DSTHTTP = DataSourceType' "HTTP"

pattern DSTRelationalDatabase :: DataSourceType
pattern DSTRelationalDatabase = DataSourceType' "RELATIONAL_DATABASE"

{-# COMPLETE
  DSTAWSLambda,
  DSTAmazonDynamodb,
  DSTAmazonElasticsearch,
  DSTNone,
  DSTHTTP,
  DSTRelationalDatabase,
  DataSourceType'
  #-}
