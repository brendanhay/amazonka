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
-- Module      : Amazonka.Config.Types.AggregatedSourceStatusType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.AggregatedSourceStatusType
  ( AggregatedSourceStatusType
      ( ..,
        AggregatedSourceStatusType_FAILED,
        AggregatedSourceStatusType_OUTDATED,
        AggregatedSourceStatusType_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AggregatedSourceStatusType = AggregatedSourceStatusType'
  { fromAggregatedSourceStatusType ::
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

pattern AggregatedSourceStatusType_FAILED :: AggregatedSourceStatusType
pattern AggregatedSourceStatusType_FAILED = AggregatedSourceStatusType' "FAILED"

pattern AggregatedSourceStatusType_OUTDATED :: AggregatedSourceStatusType
pattern AggregatedSourceStatusType_OUTDATED = AggregatedSourceStatusType' "OUTDATED"

pattern AggregatedSourceStatusType_SUCCEEDED :: AggregatedSourceStatusType
pattern AggregatedSourceStatusType_SUCCEEDED = AggregatedSourceStatusType' "SUCCEEDED"

{-# COMPLETE
  AggregatedSourceStatusType_FAILED,
  AggregatedSourceStatusType_OUTDATED,
  AggregatedSourceStatusType_SUCCEEDED,
  AggregatedSourceStatusType'
  #-}
