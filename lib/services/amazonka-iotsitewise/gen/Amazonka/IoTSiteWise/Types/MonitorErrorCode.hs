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
-- Module      : Amazonka.IoTSiteWise.Types.MonitorErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.MonitorErrorCode
  ( MonitorErrorCode
      ( ..,
        MonitorErrorCode_INTERNAL_FAILURE,
        MonitorErrorCode_LIMIT_EXCEEDED,
        MonitorErrorCode_VALIDATION_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MonitorErrorCode = MonitorErrorCode'
  { fromMonitorErrorCode ::
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

pattern MonitorErrorCode_INTERNAL_FAILURE :: MonitorErrorCode
pattern MonitorErrorCode_INTERNAL_FAILURE = MonitorErrorCode' "INTERNAL_FAILURE"

pattern MonitorErrorCode_LIMIT_EXCEEDED :: MonitorErrorCode
pattern MonitorErrorCode_LIMIT_EXCEEDED = MonitorErrorCode' "LIMIT_EXCEEDED"

pattern MonitorErrorCode_VALIDATION_ERROR :: MonitorErrorCode
pattern MonitorErrorCode_VALIDATION_ERROR = MonitorErrorCode' "VALIDATION_ERROR"

{-# COMPLETE
  MonitorErrorCode_INTERNAL_FAILURE,
  MonitorErrorCode_LIMIT_EXCEEDED,
  MonitorErrorCode_VALIDATION_ERROR,
  MonitorErrorCode'
  #-}
