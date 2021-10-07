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
-- Module      : Network.AWS.GuardDuty.Types.DataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DataSource
  ( DataSource
      ( ..,
        DataSource_CLOUD_TRAIL,
        DataSource_DNS_LOGS,
        DataSource_FLOW_LOGS,
        DataSource_S3_LOGS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DataSource = DataSource'
  { fromDataSource ::
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

pattern DataSource_CLOUD_TRAIL :: DataSource
pattern DataSource_CLOUD_TRAIL = DataSource' "CLOUD_TRAIL"

pattern DataSource_DNS_LOGS :: DataSource
pattern DataSource_DNS_LOGS = DataSource' "DNS_LOGS"

pattern DataSource_FLOW_LOGS :: DataSource
pattern DataSource_FLOW_LOGS = DataSource' "FLOW_LOGS"

pattern DataSource_S3_LOGS :: DataSource
pattern DataSource_S3_LOGS = DataSource' "S3_LOGS"

{-# COMPLETE
  DataSource_CLOUD_TRAIL,
  DataSource_DNS_LOGS,
  DataSource_FLOW_LOGS,
  DataSource_S3_LOGS,
  DataSource'
  #-}
