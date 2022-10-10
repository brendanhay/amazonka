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
-- Module      : Amazonka.DevOpsGuru.Types.CloudWatchMetricDataStatusCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.CloudWatchMetricDataStatusCode
  ( CloudWatchMetricDataStatusCode
      ( ..,
        CloudWatchMetricDataStatusCode_Complete,
        CloudWatchMetricDataStatusCode_InternalError,
        CloudWatchMetricDataStatusCode_PartialData
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CloudWatchMetricDataStatusCode = CloudWatchMetricDataStatusCode'
  { fromCloudWatchMetricDataStatusCode ::
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

pattern CloudWatchMetricDataStatusCode_Complete :: CloudWatchMetricDataStatusCode
pattern CloudWatchMetricDataStatusCode_Complete = CloudWatchMetricDataStatusCode' "Complete"

pattern CloudWatchMetricDataStatusCode_InternalError :: CloudWatchMetricDataStatusCode
pattern CloudWatchMetricDataStatusCode_InternalError = CloudWatchMetricDataStatusCode' "InternalError"

pattern CloudWatchMetricDataStatusCode_PartialData :: CloudWatchMetricDataStatusCode
pattern CloudWatchMetricDataStatusCode_PartialData = CloudWatchMetricDataStatusCode' "PartialData"

{-# COMPLETE
  CloudWatchMetricDataStatusCode_Complete,
  CloudWatchMetricDataStatusCode_InternalError,
  CloudWatchMetricDataStatusCode_PartialData,
  CloudWatchMetricDataStatusCode'
  #-}
