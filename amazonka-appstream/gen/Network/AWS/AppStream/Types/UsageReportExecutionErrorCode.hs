{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype UsageReportExecutionErrorCode = UsageReportExecutionErrorCode'
  { fromUsageReportExecutionErrorCode ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
