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
-- Module      : Network.AWS.CostAndUsageReport.Types.ReportVersioning
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types.ReportVersioning
  ( ReportVersioning
      ( ..,
        ReportVersioning_CREATE_NEW_REPORT,
        ReportVersioning_OVERWRITE_REPORT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ReportVersioning = ReportVersioning'
  { fromReportVersioning ::
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

pattern ReportVersioning_CREATE_NEW_REPORT :: ReportVersioning
pattern ReportVersioning_CREATE_NEW_REPORT = ReportVersioning' "CREATE_NEW_REPORT"

pattern ReportVersioning_OVERWRITE_REPORT :: ReportVersioning
pattern ReportVersioning_OVERWRITE_REPORT = ReportVersioning' "OVERWRITE_REPORT"

{-# COMPLETE
  ReportVersioning_CREATE_NEW_REPORT,
  ReportVersioning_OVERWRITE_REPORT,
  ReportVersioning'
  #-}
