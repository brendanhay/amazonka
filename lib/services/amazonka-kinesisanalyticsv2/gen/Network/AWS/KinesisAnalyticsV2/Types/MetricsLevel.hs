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
-- Module      : Network.AWS.KinesisAnalyticsV2.Types.MetricsLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalyticsV2.Types.MetricsLevel
  ( MetricsLevel
      ( ..,
        MetricsLevel_APPLICATION,
        MetricsLevel_OPERATOR,
        MetricsLevel_PARALLELISM,
        MetricsLevel_TASK
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype MetricsLevel = MetricsLevel'
  { fromMetricsLevel ::
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
