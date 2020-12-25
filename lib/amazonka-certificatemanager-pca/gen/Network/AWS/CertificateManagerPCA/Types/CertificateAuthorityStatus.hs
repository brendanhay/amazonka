{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityStatus
  ( CertificateAuthorityStatus
      ( CertificateAuthorityStatus',
        CertificateAuthorityStatusCreating,
        CertificateAuthorityStatusPendingCertificate,
        CertificateAuthorityStatusActive,
        CertificateAuthorityStatusDeleted,
        CertificateAuthorityStatusDisabled,
        CertificateAuthorityStatusExpired,
        CertificateAuthorityStatusFailed,
        fromCertificateAuthorityStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype CertificateAuthorityStatus = CertificateAuthorityStatus'
  { fromCertificateAuthorityStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern CertificateAuthorityStatusCreating :: CertificateAuthorityStatus
pattern CertificateAuthorityStatusCreating = CertificateAuthorityStatus' "CREATING"

pattern CertificateAuthorityStatusPendingCertificate :: CertificateAuthorityStatus
pattern CertificateAuthorityStatusPendingCertificate = CertificateAuthorityStatus' "PENDING_CERTIFICATE"

pattern CertificateAuthorityStatusActive :: CertificateAuthorityStatus
pattern CertificateAuthorityStatusActive = CertificateAuthorityStatus' "ACTIVE"

pattern CertificateAuthorityStatusDeleted :: CertificateAuthorityStatus
pattern CertificateAuthorityStatusDeleted = CertificateAuthorityStatus' "DELETED"

pattern CertificateAuthorityStatusDisabled :: CertificateAuthorityStatus
pattern CertificateAuthorityStatusDisabled = CertificateAuthorityStatus' "DISABLED"

pattern CertificateAuthorityStatusExpired :: CertificateAuthorityStatus
pattern CertificateAuthorityStatusExpired = CertificateAuthorityStatus' "EXPIRED"

pattern CertificateAuthorityStatusFailed :: CertificateAuthorityStatus
pattern CertificateAuthorityStatusFailed = CertificateAuthorityStatus' "FAILED"

{-# COMPLETE
  CertificateAuthorityStatusCreating,
  CertificateAuthorityStatusPendingCertificate,
  CertificateAuthorityStatusActive,
  CertificateAuthorityStatusDeleted,
  CertificateAuthorityStatusDisabled,
  CertificateAuthorityStatusExpired,
  CertificateAuthorityStatusFailed,
  CertificateAuthorityStatus'
  #-}
