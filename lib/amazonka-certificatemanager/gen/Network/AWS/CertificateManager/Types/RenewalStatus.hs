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
        RenewalStatusPendingAutoRenewal,
        RenewalStatusPendingValidation,
        RenewalStatusSuccess,
        RenewalStatusFailed,
        fromRenewalStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype RenewalStatus = RenewalStatus'
  { fromRenewalStatus ::
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

pattern RenewalStatusPendingAutoRenewal :: RenewalStatus
pattern RenewalStatusPendingAutoRenewal = RenewalStatus' "PENDING_AUTO_RENEWAL"

pattern RenewalStatusPendingValidation :: RenewalStatus
pattern RenewalStatusPendingValidation = RenewalStatus' "PENDING_VALIDATION"

pattern RenewalStatusSuccess :: RenewalStatus
pattern RenewalStatusSuccess = RenewalStatus' "SUCCESS"

pattern RenewalStatusFailed :: RenewalStatus
pattern RenewalStatusFailed = RenewalStatus' "FAILED"

{-# COMPLETE
  RenewalStatusPendingAutoRenewal,
  RenewalStatusPendingValidation,
  RenewalStatusSuccess,
  RenewalStatusFailed,
  RenewalStatus'
  #-}
