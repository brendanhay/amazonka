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
-- Module      : Amazonka.AlexaBusiness.Types.BusinessReportStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.BusinessReportStatus
  ( BusinessReportStatus
      ( ..,
        BusinessReportStatus_FAILED,
        BusinessReportStatus_RUNNING,
        BusinessReportStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BusinessReportStatus = BusinessReportStatus'
  { fromBusinessReportStatus ::
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

pattern BusinessReportStatus_FAILED :: BusinessReportStatus
pattern BusinessReportStatus_FAILED = BusinessReportStatus' "FAILED"

pattern BusinessReportStatus_RUNNING :: BusinessReportStatus
pattern BusinessReportStatus_RUNNING = BusinessReportStatus' "RUNNING"

pattern BusinessReportStatus_SUCCEEDED :: BusinessReportStatus
pattern BusinessReportStatus_SUCCEEDED = BusinessReportStatus' "SUCCEEDED"

{-# COMPLETE
  BusinessReportStatus_FAILED,
  BusinessReportStatus_RUNNING,
  BusinessReportStatus_SUCCEEDED,
  BusinessReportStatus'
  #-}
