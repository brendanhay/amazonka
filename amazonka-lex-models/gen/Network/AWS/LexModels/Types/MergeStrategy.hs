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
-- Module      : Network.AWS.LexModels.Types.MergeStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.MergeStrategy
  ( MergeStrategy
      ( ..,
        MergeStrategy_FAIL_ON_CONFLICT,
        MergeStrategy_OVERWRITE_LATEST
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MergeStrategy = MergeStrategy'
  { fromMergeStrategy ::
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

pattern MergeStrategy_FAIL_ON_CONFLICT :: MergeStrategy
pattern MergeStrategy_FAIL_ON_CONFLICT = MergeStrategy' "FAIL_ON_CONFLICT"

pattern MergeStrategy_OVERWRITE_LATEST :: MergeStrategy
pattern MergeStrategy_OVERWRITE_LATEST = MergeStrategy' "OVERWRITE_LATEST"

{-# COMPLETE
  MergeStrategy_FAIL_ON_CONFLICT,
  MergeStrategy_OVERWRITE_LATEST,
  MergeStrategy'
  #-}
