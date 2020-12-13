{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.RenewalStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.RenewalStatus
  ( RenewalStatus
      ( RenewalStatus',
        RSPendingAutoRenewal,
        RSPendingValidation,
        RSSuccess,
        RSFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RenewalStatus = RenewalStatus' Lude.Text
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

pattern RSPendingAutoRenewal :: RenewalStatus
pattern RSPendingAutoRenewal = RenewalStatus' "PENDING_AUTO_RENEWAL"

pattern RSPendingValidation :: RenewalStatus
pattern RSPendingValidation = RenewalStatus' "PENDING_VALIDATION"

pattern RSSuccess :: RenewalStatus
pattern RSSuccess = RenewalStatus' "SUCCESS"

pattern RSFailed :: RenewalStatus
pattern RSFailed = RenewalStatus' "FAILED"

{-# COMPLETE
  RSPendingAutoRenewal,
  RSPendingValidation,
  RSSuccess,
  RSFailed,
  RenewalStatus'
  #-}
