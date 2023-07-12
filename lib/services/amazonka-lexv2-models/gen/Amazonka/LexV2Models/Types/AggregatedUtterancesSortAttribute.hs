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
-- Module      : Amazonka.LexV2Models.Types.AggregatedUtterancesSortAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.AggregatedUtterancesSortAttribute
  ( AggregatedUtterancesSortAttribute
      ( ..,
        AggregatedUtterancesSortAttribute_HitCount,
        AggregatedUtterancesSortAttribute_MissedCount
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AggregatedUtterancesSortAttribute = AggregatedUtterancesSortAttribute'
  { fromAggregatedUtterancesSortAttribute ::
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

pattern AggregatedUtterancesSortAttribute_HitCount :: AggregatedUtterancesSortAttribute
pattern AggregatedUtterancesSortAttribute_HitCount = AggregatedUtterancesSortAttribute' "HitCount"

pattern AggregatedUtterancesSortAttribute_MissedCount :: AggregatedUtterancesSortAttribute
pattern AggregatedUtterancesSortAttribute_MissedCount = AggregatedUtterancesSortAttribute' "MissedCount"

{-# COMPLETE
  AggregatedUtterancesSortAttribute_HitCount,
  AggregatedUtterancesSortAttribute_MissedCount,
  AggregatedUtterancesSortAttribute'
  #-}
