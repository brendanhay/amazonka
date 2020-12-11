-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RenewalStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RenewalStatus
  ( RenewalStatus
      ( RenewalStatus',
        RSFailed,
        RSPendingAutoRenewal,
        RSPendingValidation,
        RSSuccess
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

pattern RSFailed :: RenewalStatus
pattern RSFailed = RenewalStatus' "Failed"

pattern RSPendingAutoRenewal :: RenewalStatus
pattern RSPendingAutoRenewal = RenewalStatus' "PendingAutoRenewal"

pattern RSPendingValidation :: RenewalStatus
pattern RSPendingValidation = RenewalStatus' "PendingValidation"

pattern RSSuccess :: RenewalStatus
pattern RSSuccess = RenewalStatus' "Success"

{-# COMPLETE
  RSFailed,
  RSPendingAutoRenewal,
  RSPendingValidation,
  RSSuccess,
  RenewalStatus'
  #-}
