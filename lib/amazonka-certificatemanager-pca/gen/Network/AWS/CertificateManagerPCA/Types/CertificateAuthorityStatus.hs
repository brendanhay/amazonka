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
        Active,
        Creating,
        Deleted,
        Disabled,
        Expired,
        Failed,
        PendingCertificate
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CertificateAuthorityStatus = CertificateAuthorityStatus' Lude.Text
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

pattern Active :: CertificateAuthorityStatus
pattern Active = CertificateAuthorityStatus' "ACTIVE"

pattern Creating :: CertificateAuthorityStatus
pattern Creating = CertificateAuthorityStatus' "CREATING"

pattern Deleted :: CertificateAuthorityStatus
pattern Deleted = CertificateAuthorityStatus' "DELETED"

pattern Disabled :: CertificateAuthorityStatus
pattern Disabled = CertificateAuthorityStatus' "DISABLED"

pattern Expired :: CertificateAuthorityStatus
pattern Expired = CertificateAuthorityStatus' "EXPIRED"

pattern Failed :: CertificateAuthorityStatus
pattern Failed = CertificateAuthorityStatus' "FAILED"

pattern PendingCertificate :: CertificateAuthorityStatus
pattern PendingCertificate = CertificateAuthorityStatus' "PENDING_CERTIFICATE"

{-# COMPLETE
  Active,
  Creating,
  Deleted,
  Disabled,
  Expired,
  Failed,
  PendingCertificate,
  CertificateAuthorityStatus'
  #-}
