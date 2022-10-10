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
-- Module      : Amazonka.Kafka.Types.EnhancedMonitoring
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.EnhancedMonitoring
  ( EnhancedMonitoring
      ( ..,
        EnhancedMonitoring_DEFAULT,
        EnhancedMonitoring_PER_BROKER,
        EnhancedMonitoring_PER_TOPIC_PER_BROKER,
        EnhancedMonitoring_PER_TOPIC_PER_PARTITION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Specifies which metrics are gathered for the MSK cluster. This property
-- has the following possible values: DEFAULT, PER_BROKER,
-- PER_TOPIC_PER_BROKER, and PER_TOPIC_PER_PARTITION. For a list of the
-- metrics associated with each of these levels of monitoring, see
-- <https://docs.aws.amazon.com/msk/latest/developerguide/monitoring.html Monitoring>.
newtype EnhancedMonitoring = EnhancedMonitoring'
  { fromEnhancedMonitoring ::
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

pattern EnhancedMonitoring_DEFAULT :: EnhancedMonitoring
pattern EnhancedMonitoring_DEFAULT = EnhancedMonitoring' "DEFAULT"

pattern EnhancedMonitoring_PER_BROKER :: EnhancedMonitoring
pattern EnhancedMonitoring_PER_BROKER = EnhancedMonitoring' "PER_BROKER"

pattern EnhancedMonitoring_PER_TOPIC_PER_BROKER :: EnhancedMonitoring
pattern EnhancedMonitoring_PER_TOPIC_PER_BROKER = EnhancedMonitoring' "PER_TOPIC_PER_BROKER"

pattern EnhancedMonitoring_PER_TOPIC_PER_PARTITION :: EnhancedMonitoring
pattern EnhancedMonitoring_PER_TOPIC_PER_PARTITION = EnhancedMonitoring' "PER_TOPIC_PER_PARTITION"

{-# COMPLETE
  EnhancedMonitoring_DEFAULT,
  EnhancedMonitoring_PER_BROKER,
  EnhancedMonitoring_PER_TOPIC_PER_BROKER,
  EnhancedMonitoring_PER_TOPIC_PER_PARTITION,
  EnhancedMonitoring'
  #-}
