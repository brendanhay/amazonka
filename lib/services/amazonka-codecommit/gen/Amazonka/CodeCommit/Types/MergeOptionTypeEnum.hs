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
-- Module      : Amazonka.CodeCommit.Types.MergeOptionTypeEnum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.MergeOptionTypeEnum
  ( MergeOptionTypeEnum
      ( ..,
        MergeOptionTypeEnum_FAST_FORWARD_MERGE,
        MergeOptionTypeEnum_SQUASH_MERGE,
        MergeOptionTypeEnum_THREE_WAY_MERGE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MergeOptionTypeEnum = MergeOptionTypeEnum'
  { fromMergeOptionTypeEnum ::
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
