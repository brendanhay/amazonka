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
-- Module      : Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityStatus
  ( CertificateAuthorityStatus
      ( ..,
        CertificateAuthorityStatus_ACTIVE,
        CertificateAuthorityStatus_CREATING,
        CertificateAuthorityStatus_DELETED,
        CertificateAuthorityStatus_DISABLED,
        CertificateAuthorityStatus_EXPIRED,
        CertificateAuthorityStatus_FAILED,
        CertificateAuthorityStatus_PENDING_CERTIFICATE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype CertificateAuthorityStatus = CertificateAuthorityStatus'
  { fromCertificateAuthorityStatus ::
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

pattern CertificateAuthorityStatus_ACTIVE :: CertificateAuthorityStatus
pattern CertificateAuthorityStatus_ACTIVE = CertificateAuthorityStatus' "ACTIVE"

pattern CertificateAuthorityStatus_CREATING :: CertificateAuthorityStatus
pattern CertificateAuthorityStatus_CREATING = CertificateAuthorityStatus' "CREATING"

pattern CertificateAuthorityStatus_DELETED :: CertificateAuthorityStatus
pattern CertificateAuthorityStatus_DELETED = CertificateAuthorityStatus' "DELETED"

pattern CertificateAuthorityStatus_DISABLED :: CertificateAuthorityStatus
pattern CertificateAuthorityStatus_DISABLED = CertificateAuthorityStatus' "DISABLED"

pattern CertificateAuthorityStatus_EXPIRED :: CertificateAuthorityStatus
pattern CertificateAuthorityStatus_EXPIRED = CertificateAuthorityStatus' "EXPIRED"

pattern CertificateAuthorityStatus_FAILED :: CertificateAuthorityStatus
pattern CertificateAuthorityStatus_FAILED = CertificateAuthorityStatus' "FAILED"

pattern CertificateAuthorityStatus_PENDING_CERTIFICATE :: CertificateAuthorityStatus
pattern CertificateAuthorityStatus_PENDING_CERTIFICATE = CertificateAuthorityStatus' "PENDING_CERTIFICATE"

{-# COMPLETE
  CertificateAuthorityStatus_ACTIVE,
  CertificateAuthorityStatus_CREATING,
  CertificateAuthorityStatus_DELETED,
  CertificateAuthorityStatus_DISABLED,
  CertificateAuthorityStatus_EXPIRED,
  CertificateAuthorityStatus_FAILED,
  CertificateAuthorityStatus_PENDING_CERTIFICATE,
  CertificateAuthorityStatus'
  #-}
