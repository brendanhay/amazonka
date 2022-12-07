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
-- Module      : Amazonka.LicenseManager.Types.LicenseConversionTaskStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.LicenseConversionTaskStatus
  ( LicenseConversionTaskStatus
      ( ..,
        LicenseConversionTaskStatus_FAILED,
        LicenseConversionTaskStatus_IN_PROGRESS,
        LicenseConversionTaskStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LicenseConversionTaskStatus = LicenseConversionTaskStatus'
  { fromLicenseConversionTaskStatus ::
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
