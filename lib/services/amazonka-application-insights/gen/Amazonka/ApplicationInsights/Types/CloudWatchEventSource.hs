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
-- Module      : Amazonka.ApplicationInsights.Types.CloudWatchEventSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationInsights.Types.CloudWatchEventSource
  ( CloudWatchEventSource
      ( ..,
        CloudWatchEventSource_CODE_DEPLOY,
        CloudWatchEventSource_EC2,
        CloudWatchEventSource_HEALTH,
        CloudWatchEventSource_RDS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CloudWatchEventSource = CloudWatchEventSource'
  { fromCloudWatchEventSource ::
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

pattern CloudWatchEventSource_CODE_DEPLOY :: CloudWatchEventSource
pattern CloudWatchEventSource_CODE_DEPLOY = CloudWatchEventSource' "CODE_DEPLOY"

pattern CloudWatchEventSource_EC2 :: CloudWatchEventSource
pattern CloudWatchEventSource_EC2 = CloudWatchEventSource' "EC2"

pattern CloudWatchEventSource_HEALTH :: CloudWatchEventSource
pattern CloudWatchEventSource_HEALTH = CloudWatchEventSource' "HEALTH"

pattern CloudWatchEventSource_RDS :: CloudWatchEventSource
pattern CloudWatchEventSource_RDS = CloudWatchEventSource' "RDS"

{-# COMPLETE
  CloudWatchEventSource_CODE_DEPLOY,
  CloudWatchEventSource_EC2,
  CloudWatchEventSource_HEALTH,
  CloudWatchEventSource_RDS,
  CloudWatchEventSource'
  #-}
