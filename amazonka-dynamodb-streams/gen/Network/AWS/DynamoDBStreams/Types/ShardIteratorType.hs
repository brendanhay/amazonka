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
-- Module      : Network.AWS.DynamoDBStreams.Types.ShardIteratorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.ShardIteratorType
  ( ShardIteratorType
      ( ..,
        ShardIteratorType_AFTER_SEQUENCE_NUMBER,
        ShardIteratorType_AT_SEQUENCE_NUMBER,
        ShardIteratorType_LATEST,
        ShardIteratorType_TRIM_HORIZON
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ShardIteratorType = ShardIteratorType'
  { fromShardIteratorType ::
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

pattern ShardIteratorType_AFTER_SEQUENCE_NUMBER :: ShardIteratorType
pattern ShardIteratorType_AFTER_SEQUENCE_NUMBER = ShardIteratorType' "AFTER_SEQUENCE_NUMBER"

pattern ShardIteratorType_AT_SEQUENCE_NUMBER :: ShardIteratorType
pattern ShardIteratorType_AT_SEQUENCE_NUMBER = ShardIteratorType' "AT_SEQUENCE_NUMBER"

pattern ShardIteratorType_LATEST :: ShardIteratorType
pattern ShardIteratorType_LATEST = ShardIteratorType' "LATEST"

pattern ShardIteratorType_TRIM_HORIZON :: ShardIteratorType
pattern ShardIteratorType_TRIM_HORIZON = ShardIteratorType' "TRIM_HORIZON"

{-# COMPLETE
  ShardIteratorType_AFTER_SEQUENCE_NUMBER,
  ShardIteratorType_AT_SEQUENCE_NUMBER,
  ShardIteratorType_LATEST,
  ShardIteratorType_TRIM_HORIZON,
  ShardIteratorType'
  #-}
