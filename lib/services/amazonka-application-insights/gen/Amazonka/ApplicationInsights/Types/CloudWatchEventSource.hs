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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CloudWatchEventSource = CloudWatchEventSource'
  { fromCloudWatchEventSource ::
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
