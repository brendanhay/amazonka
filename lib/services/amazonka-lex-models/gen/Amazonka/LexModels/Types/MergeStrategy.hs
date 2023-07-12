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
-- Module      : Amazonka.LexModels.Types.MergeStrategy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.MergeStrategy
  ( MergeStrategy
      ( ..,
        MergeStrategy_FAIL_ON_CONFLICT,
        MergeStrategy_OVERWRITE_LATEST
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MergeStrategy = MergeStrategy'
  { fromMergeStrategy ::
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

pattern MergeStrategy_FAIL_ON_CONFLICT :: MergeStrategy
pattern MergeStrategy_FAIL_ON_CONFLICT = MergeStrategy' "FAIL_ON_CONFLICT"

pattern MergeStrategy_OVERWRITE_LATEST :: MergeStrategy
pattern MergeStrategy_OVERWRITE_LATEST = MergeStrategy' "OVERWRITE_LATEST"

{-# COMPLETE
  MergeStrategy_FAIL_ON_CONFLICT,
  MergeStrategy_OVERWRITE_LATEST,
  MergeStrategy'
  #-}
