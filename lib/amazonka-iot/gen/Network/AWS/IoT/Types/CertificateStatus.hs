{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CertificateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CertificateStatus
  ( CertificateStatus
      ( CertificateStatus',
        CertificateStatusActive,
        CertificateStatusInactive,
        CertificateStatusRevoked,
        CertificateStatusPendingTransfer,
        CertificateStatusRegisterInactive,
        CertificateStatusPendingActivation,
        fromCertificateStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype CertificateStatus = CertificateStatus'
  { fromCertificateStatus ::
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

pattern CertificateStatusActive :: CertificateStatus
pattern CertificateStatusActive = CertificateStatus' "ACTIVE"

pattern CertificateStatusInactive :: CertificateStatus
pattern CertificateStatusInactive = CertificateStatus' "INACTIVE"

pattern CertificateStatusRevoked :: CertificateStatus
pattern CertificateStatusRevoked = CertificateStatus' "REVOKED"

pattern CertificateStatusPendingTransfer :: CertificateStatus
pattern CertificateStatusPendingTransfer = CertificateStatus' "PENDING_TRANSFER"

pattern CertificateStatusRegisterInactive :: CertificateStatus
pattern CertificateStatusRegisterInactive = CertificateStatus' "REGISTER_INACTIVE"

pattern CertificateStatusPendingActivation :: CertificateStatus
pattern CertificateStatusPendingActivation = CertificateStatus' "PENDING_ACTIVATION"

{-# COMPLETE
  CertificateStatusActive,
  CertificateStatusInactive,
  CertificateStatusRevoked,
  CertificateStatusPendingTransfer,
  CertificateStatusRegisterInactive,
  CertificateStatusPendingActivation,
  CertificateStatus'
  #-}
