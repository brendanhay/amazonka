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
-- Module      : Amazonka.AppStream.Types.UsageReportExecutionErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.UsageReportExecutionErrorCode
  ( UsageReportExecutionErrorCode
      ( ..,
        UsageReportExecutionErrorCode_ACCESS_DENIED,
        UsageReportExecutionErrorCode_INTERNAL_SERVICE_ERROR,
        UsageReportExecutionErrorCode_RESOURCE_NOT_FOUND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UsageReportExecutionErrorCode = UsageReportExecutionErrorCode'
  { fromUsageReportExecutionErrorCode ::
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

pattern UsageReportExecutionErrorCode_ACCESS_DENIED :: UsageReportExecutionErrorCode
pattern UsageReportExecutionErrorCode_ACCESS_DENIED = UsageReportExecutionErrorCode' "ACCESS_DENIED"

pattern UsageReportExecutionErrorCode_INTERNAL_SERVICE_ERROR :: UsageReportExecutionErrorCode
pattern UsageReportExecutionErrorCode_INTERNAL_SERVICE_ERROR = UsageReportExecutionErrorCode' "INTERNAL_SERVICE_ERROR"

pattern UsageReportExecutionErrorCode_RESOURCE_NOT_FOUND :: UsageReportExecutionErrorCode
pattern UsageReportExecutionErrorCode_RESOURCE_NOT_FOUND = UsageReportExecutionErrorCode' "RESOURCE_NOT_FOUND"

{-# COMPLETE
  UsageReportExecutionErrorCode_ACCESS_DENIED,
  UsageReportExecutionErrorCode_INTERNAL_SERVICE_ERROR,
  UsageReportExecutionErrorCode_RESOURCE_NOT_FOUND,
  UsageReportExecutionErrorCode'
  #-}
