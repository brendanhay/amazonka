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
-- Module      : Network.AWS.CertificateManagerPCA.Types.AuditReportStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.AuditReportStatus
  ( AuditReportStatus
      ( ..,
        AuditReportStatus_CREATING,
        AuditReportStatus_FAILED,
        AuditReportStatus_SUCCESS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AuditReportStatus = AuditReportStatus'
  { fromAuditReportStatus ::
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
