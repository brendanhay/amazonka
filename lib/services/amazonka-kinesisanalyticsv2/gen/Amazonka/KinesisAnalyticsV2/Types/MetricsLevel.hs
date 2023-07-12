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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.MetricsLevel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.MetricsLevel
  ( MetricsLevel
      ( ..,
        MetricsLevel_APPLICATION,
        MetricsLevel_OPERATOR,
        MetricsLevel_PARALLELISM,
        MetricsLevel_TASK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MetricsLevel = MetricsLevel'
  { fromMetricsLevel ::
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

pattern MetricsLevel_APPLICATION :: MetricsLevel
pattern MetricsLevel_APPLICATION = MetricsLevel' "APPLICATION"

pattern MetricsLevel_OPERATOR :: MetricsLevel
pattern MetricsLevel_OPERATOR = MetricsLevel' "OPERATOR"

pattern MetricsLevel_PARALLELISM :: MetricsLevel
pattern MetricsLevel_PARALLELISM = MetricsLevel' "PARALLELISM"

pattern MetricsLevel_TASK :: MetricsLevel
pattern MetricsLevel_TASK = MetricsLevel' "TASK"

{-# COMPLETE
  MetricsLevel_APPLICATION,
  MetricsLevel_OPERATOR,
  MetricsLevel_PARALLELISM,
  MetricsLevel_TASK,
  MetricsLevel'
  #-}
