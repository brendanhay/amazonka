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
        CSActive,
        CSInactive,
        CSPendingActivation,
        CSPendingTransfer,
        CSRegisterInactive,
        CSRevoked
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CertificateStatus = CertificateStatus' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CSActive :: CertificateStatus
pattern CSActive = CertificateStatus' "ACTIVE"

pattern CSInactive :: CertificateStatus
pattern CSInactive = CertificateStatus' "INACTIVE"

pattern CSPendingActivation :: CertificateStatus
pattern CSPendingActivation = CertificateStatus' "PENDING_ACTIVATION"

pattern CSPendingTransfer :: CertificateStatus
pattern CSPendingTransfer = CertificateStatus' "PENDING_TRANSFER"

pattern CSRegisterInactive :: CertificateStatus
pattern CSRegisterInactive = CertificateStatus' "REGISTER_INACTIVE"

pattern CSRevoked :: CertificateStatus
pattern CSRevoked = CertificateStatus' "REVOKED"

{-# COMPLETE
  CSActive,
  CSInactive,
  CSPendingActivation,
  CSPendingTransfer,
  CSRegisterInactive,
  CSRevoked,
  CertificateStatus'
  #-}
