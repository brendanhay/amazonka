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
-- Module      : Amazonka.ComputeOptimizer.Types.InferredWorkloadType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.InferredWorkloadType
  ( InferredWorkloadType
      ( ..,
        InferredWorkloadType_AmazonEmr,
        InferredWorkloadType_ApacheCassandra,
        InferredWorkloadType_ApacheHadoop,
        InferredWorkloadType_Memcached,
        InferredWorkloadType_Nginx,
        InferredWorkloadType_PostgreSql,
        InferredWorkloadType_Redis
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype InferredWorkloadType = InferredWorkloadType'
  { fromInferredWorkloadType ::
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

pattern InferredWorkloadType_AmazonEmr :: InferredWorkloadType
pattern InferredWorkloadType_AmazonEmr = InferredWorkloadType' "AmazonEmr"

pattern InferredWorkloadType_ApacheCassandra :: InferredWorkloadType
pattern InferredWorkloadType_ApacheCassandra = InferredWorkloadType' "ApacheCassandra"

pattern InferredWorkloadType_ApacheHadoop :: InferredWorkloadType
pattern InferredWorkloadType_ApacheHadoop = InferredWorkloadType' "ApacheHadoop"

pattern InferredWorkloadType_Memcached :: InferredWorkloadType
pattern InferredWorkloadType_Memcached = InferredWorkloadType' "Memcached"

pattern InferredWorkloadType_Nginx :: InferredWorkloadType
pattern InferredWorkloadType_Nginx = InferredWorkloadType' "Nginx"

pattern InferredWorkloadType_PostgreSql :: InferredWorkloadType
pattern InferredWorkloadType_PostgreSql = InferredWorkloadType' "PostgreSql"

pattern InferredWorkloadType_Redis :: InferredWorkloadType
pattern InferredWorkloadType_Redis = InferredWorkloadType' "Redis"

{-# COMPLETE
  InferredWorkloadType_AmazonEmr,
  InferredWorkloadType_ApacheCassandra,
  InferredWorkloadType_ApacheHadoop,
  InferredWorkloadType_Memcached,
  InferredWorkloadType_Nginx,
  InferredWorkloadType_PostgreSql,
  InferredWorkloadType_Redis,
  InferredWorkloadType'
  #-}
