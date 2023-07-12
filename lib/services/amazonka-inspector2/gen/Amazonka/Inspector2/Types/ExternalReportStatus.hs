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
-- Module      : Amazonka.Inspector2.Types.ExternalReportStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.ExternalReportStatus
  ( ExternalReportStatus
      ( ..,
        ExternalReportStatus_CANCELLED,
        ExternalReportStatus_FAILED,
        ExternalReportStatus_IN_PROGRESS,
        ExternalReportStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExternalReportStatus = ExternalReportStatus'
  { fromExternalReportStatus ::
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

pattern ExternalReportStatus_CANCELLED :: ExternalReportStatus
pattern ExternalReportStatus_CANCELLED = ExternalReportStatus' "CANCELLED"

pattern ExternalReportStatus_FAILED :: ExternalReportStatus
pattern ExternalReportStatus_FAILED = ExternalReportStatus' "FAILED"

pattern ExternalReportStatus_IN_PROGRESS :: ExternalReportStatus
pattern ExternalReportStatus_IN_PROGRESS = ExternalReportStatus' "IN_PROGRESS"

pattern ExternalReportStatus_SUCCEEDED :: ExternalReportStatus
pattern ExternalReportStatus_SUCCEEDED = ExternalReportStatus' "SUCCEEDED"

{-# COMPLETE
  ExternalReportStatus_CANCELLED,
  ExternalReportStatus_FAILED,
  ExternalReportStatus_IN_PROGRESS,
  ExternalReportStatus_SUCCEEDED,
  ExternalReportStatus'
  #-}
