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
-- Module      : Network.AWS.Lightsail.Types.CertificateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CertificateStatus
  ( CertificateStatus
      ( ..,
        CertificateStatus_EXPIRED,
        CertificateStatus_FAILED,
        CertificateStatus_INACTIVE,
        CertificateStatus_ISSUED,
        CertificateStatus_PENDING_VALIDATION,
        CertificateStatus_REVOKED,
        CertificateStatus_VALIDATION_TIMED_OUT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype CertificateStatus = CertificateStatus'
  { fromCertificateStatus ::
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

pattern CertificateStatus_EXPIRED :: CertificateStatus
pattern CertificateStatus_EXPIRED = CertificateStatus' "EXPIRED"

pattern CertificateStatus_FAILED :: CertificateStatus
pattern CertificateStatus_FAILED = CertificateStatus' "FAILED"

pattern CertificateStatus_INACTIVE :: CertificateStatus
pattern CertificateStatus_INACTIVE = CertificateStatus' "INACTIVE"

pattern CertificateStatus_ISSUED :: CertificateStatus
pattern CertificateStatus_ISSUED = CertificateStatus' "ISSUED"

pattern CertificateStatus_PENDING_VALIDATION :: CertificateStatus
pattern CertificateStatus_PENDING_VALIDATION = CertificateStatus' "PENDING_VALIDATION"

pattern CertificateStatus_REVOKED :: CertificateStatus
pattern CertificateStatus_REVOKED = CertificateStatus' "REVOKED"

pattern CertificateStatus_VALIDATION_TIMED_OUT :: CertificateStatus
pattern CertificateStatus_VALIDATION_TIMED_OUT = CertificateStatus' "VALIDATION_TIMED_OUT"

{-# COMPLETE
  CertificateStatus_EXPIRED,
  CertificateStatus_FAILED,
  CertificateStatus_INACTIVE,
  CertificateStatus_ISSUED,
  CertificateStatus_PENDING_VALIDATION,
  CertificateStatus_REVOKED,
  CertificateStatus_VALIDATION_TIMED_OUT,
  CertificateStatus'
  #-}
