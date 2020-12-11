-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ByoipCidrState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ByoipCidrState
  ( ByoipCidrState
      ( ByoipCidrState',
        Advertised,
        Deprovisioned,
        FailedDeprovision,
        FailedProvision,
        PendingDeprovision,
        PendingProvision,
        Provisioned,
        ProvisionedNotPubliclyAdvertisable
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ByoipCidrState = ByoipCidrState' Lude.Text
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

pattern Advertised :: ByoipCidrState
pattern Advertised = ByoipCidrState' "advertised"

pattern Deprovisioned :: ByoipCidrState
pattern Deprovisioned = ByoipCidrState' "deprovisioned"

pattern FailedDeprovision :: ByoipCidrState
pattern FailedDeprovision = ByoipCidrState' "failed-deprovision"

pattern FailedProvision :: ByoipCidrState
pattern FailedProvision = ByoipCidrState' "failed-provision"

pattern PendingDeprovision :: ByoipCidrState
pattern PendingDeprovision = ByoipCidrState' "pending-deprovision"

pattern PendingProvision :: ByoipCidrState
pattern PendingProvision = ByoipCidrState' "pending-provision"

pattern Provisioned :: ByoipCidrState
pattern Provisioned = ByoipCidrState' "provisioned"

pattern ProvisionedNotPubliclyAdvertisable :: ByoipCidrState
pattern ProvisionedNotPubliclyAdvertisable = ByoipCidrState' "provisioned-not-publicly-advertisable"

{-# COMPLETE
  Advertised,
  Deprovisioned,
  FailedDeprovision,
  FailedProvision,
  PendingDeprovision,
  PendingProvision,
  Provisioned,
  ProvisionedNotPubliclyAdvertisable,
  ByoipCidrState'
  #-}
