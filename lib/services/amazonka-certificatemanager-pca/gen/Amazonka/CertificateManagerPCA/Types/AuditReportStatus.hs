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
-- Module      : Amazonka.CertificateManagerPCA.Types.AuditReportStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.AuditReportStatus
  ( AuditReportStatus
      ( ..,
        AuditReportStatus_CREATING,
        AuditReportStatus_FAILED,
        AuditReportStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AuditReportStatus = AuditReportStatus'
  { fromAuditReportStatus ::
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

pattern AuditReportStatus_CREATING :: AuditReportStatus
pattern AuditReportStatus_CREATING = AuditReportStatus' "CREATING"

pattern AuditReportStatus_FAILED :: AuditReportStatus
pattern AuditReportStatus_FAILED = AuditReportStatus' "FAILED"

pattern AuditReportStatus_SUCCESS :: AuditReportStatus
pattern AuditReportStatus_SUCCESS = AuditReportStatus' "SUCCESS"

{-# COMPLETE
  AuditReportStatus_CREATING,
  AuditReportStatus_FAILED,
  AuditReportStatus_SUCCESS,
  AuditReportStatus'
  #-}
