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
-- Module      : Network.AWS.AppStream.Types.UsageReportExecutionErrorCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.UsageReportExecutionErrorCode
  ( UsageReportExecutionErrorCode
      ( ..,
        UsageReportExecutionErrorCode_ACCESS_DENIED,
        UsageReportExecutionErrorCode_INTERNAL_SERVICE_ERROR,
        UsageReportExecutionErrorCode_RESOURCE_NOT_FOUND
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype UsageReportExecutionErrorCode = UsageReportExecutionErrorCode'
  { fromUsageReportExecutionErrorCode ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
