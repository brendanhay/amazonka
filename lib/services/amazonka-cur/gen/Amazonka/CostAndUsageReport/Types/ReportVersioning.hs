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
-- Module      : Amazonka.CostAndUsageReport.Types.ReportVersioning
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostAndUsageReport.Types.ReportVersioning
  ( ReportVersioning
      ( ..,
        ReportVersioning_CREATE_NEW_REPORT,
        ReportVersioning_OVERWRITE_REPORT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReportVersioning = ReportVersioning'
  { fromReportVersioning ::
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

pattern ReportVersioning_CREATE_NEW_REPORT :: ReportVersioning
pattern ReportVersioning_CREATE_NEW_REPORT = ReportVersioning' "CREATE_NEW_REPORT"

pattern ReportVersioning_OVERWRITE_REPORT :: ReportVersioning
pattern ReportVersioning_OVERWRITE_REPORT = ReportVersioning' "OVERWRITE_REPORT"

{-# COMPLETE
  ReportVersioning_CREATE_NEW_REPORT,
  ReportVersioning_OVERWRITE_REPORT,
  ReportVersioning'
  #-}
