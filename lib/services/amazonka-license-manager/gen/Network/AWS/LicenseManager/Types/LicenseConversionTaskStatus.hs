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
-- Module      : Network.AWS.LicenseManager.Types.LicenseConversionTaskStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LicenseManager.Types.LicenseConversionTaskStatus
  ( LicenseConversionTaskStatus
      ( ..,
        LicenseConversionTaskStatus_FAILED,
        LicenseConversionTaskStatus_IN_PROGRESS,
        LicenseConversionTaskStatus_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype LicenseConversionTaskStatus = LicenseConversionTaskStatus'
  { fromLicenseConversionTaskStatus ::
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

pattern LicenseConversionTaskStatus_FAILED :: LicenseConversionTaskStatus
pattern LicenseConversionTaskStatus_FAILED = LicenseConversionTaskStatus' "FAILED"

pattern LicenseConversionTaskStatus_IN_PROGRESS :: LicenseConversionTaskStatus
pattern LicenseConversionTaskStatus_IN_PROGRESS = LicenseConversionTaskStatus' "IN_PROGRESS"

pattern LicenseConversionTaskStatus_SUCCEEDED :: LicenseConversionTaskStatus
pattern LicenseConversionTaskStatus_SUCCEEDED = LicenseConversionTaskStatus' "SUCCEEDED"

{-# COMPLETE
  LicenseConversionTaskStatus_FAILED,
  LicenseConversionTaskStatus_IN_PROGRESS,
  LicenseConversionTaskStatus_SUCCEEDED,
  LicenseConversionTaskStatus'
  #-}
