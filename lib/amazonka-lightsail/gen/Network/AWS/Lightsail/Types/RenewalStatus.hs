{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        RPendingAutoRenewal,
        RPendingValidation,
        RSuccess,
        RFailed
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

pattern RPendingAutoRenewal :: RenewalStatus
pattern RPendingAutoRenewal = RenewalStatus' "PendingAutoRenewal"

pattern RPendingValidation :: RenewalStatus
pattern RPendingValidation = RenewalStatus' "PendingValidation"

pattern RSuccess :: RenewalStatus
pattern RSuccess = RenewalStatus' "Success"

pattern RFailed :: RenewalStatus
pattern RFailed = RenewalStatus' "Failed"

{-# COMPLETE
  RPendingAutoRenewal,
  RPendingValidation,
  RSuccess,
  RFailed,
  RenewalStatus'
  #-}
