{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.RevocationReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.RevocationReason
  ( RevocationReason
      ( RevocationReason',
        Unspecified,
        KeyCompromise,
        CaCompromise,
        AffiliationChanged,
        Superceded,
        CessationOfOperation,
        CertificateHold,
        RemoveFromCrl,
        PrivilegeWithdrawn,
        AACompromise
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RevocationReason = RevocationReason' Lude.Text
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

pattern Unspecified :: RevocationReason
pattern Unspecified = RevocationReason' "UNSPECIFIED"

pattern KeyCompromise :: RevocationReason
pattern KeyCompromise = RevocationReason' "KEY_COMPROMISE"

pattern CaCompromise :: RevocationReason
pattern CaCompromise = RevocationReason' "CA_COMPROMISE"

pattern AffiliationChanged :: RevocationReason
pattern AffiliationChanged = RevocationReason' "AFFILIATION_CHANGED"

pattern Superceded :: RevocationReason
pattern Superceded = RevocationReason' "SUPERCEDED"

pattern CessationOfOperation :: RevocationReason
pattern CessationOfOperation = RevocationReason' "CESSATION_OF_OPERATION"

pattern CertificateHold :: RevocationReason
pattern CertificateHold = RevocationReason' "CERTIFICATE_HOLD"

pattern RemoveFromCrl :: RevocationReason
pattern RemoveFromCrl = RevocationReason' "REMOVE_FROM_CRL"

pattern PrivilegeWithdrawn :: RevocationReason
pattern PrivilegeWithdrawn = RevocationReason' "PRIVILEGE_WITHDRAWN"

pattern AACompromise :: RevocationReason
pattern AACompromise = RevocationReason' "A_A_COMPROMISE"

{-# COMPLETE
  Unspecified,
  KeyCompromise,
  CaCompromise,
  AffiliationChanged,
  Superceded,
  CessationOfOperation,
  CertificateHold,
  RemoveFromCrl,
  PrivilegeWithdrawn,
  AACompromise,
  RevocationReason'
  #-}
