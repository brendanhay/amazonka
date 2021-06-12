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
-- Module      : Network.AWS.CodeCommit.Types.MergeOptionTypeEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.MergeOptionTypeEnum
  ( MergeOptionTypeEnum
      ( ..,
        MergeOptionTypeEnum_FAST_FORWARD_MERGE,
        MergeOptionTypeEnum_SQUASH_MERGE,
        MergeOptionTypeEnum_THREE_WAY_MERGE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MergeOptionTypeEnum = MergeOptionTypeEnum'
  { fromMergeOptionTypeEnum ::
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

pattern MergeOptionTypeEnum_FAST_FORWARD_MERGE :: MergeOptionTypeEnum
pattern MergeOptionTypeEnum_FAST_FORWARD_MERGE = MergeOptionTypeEnum' "FAST_FORWARD_MERGE"

pattern MergeOptionTypeEnum_SQUASH_MERGE :: MergeOptionTypeEnum
pattern MergeOptionTypeEnum_SQUASH_MERGE = MergeOptionTypeEnum' "SQUASH_MERGE"

pattern MergeOptionTypeEnum_THREE_WAY_MERGE :: MergeOptionTypeEnum
pattern MergeOptionTypeEnum_THREE_WAY_MERGE = MergeOptionTypeEnum' "THREE_WAY_MERGE"

{-# COMPLETE
  MergeOptionTypeEnum_FAST_FORWARD_MERGE,
  MergeOptionTypeEnum_SQUASH_MERGE,
  MergeOptionTypeEnum_THREE_WAY_MERGE,
  MergeOptionTypeEnum'
  #-}
