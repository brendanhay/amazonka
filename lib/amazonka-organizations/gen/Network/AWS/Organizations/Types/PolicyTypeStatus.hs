-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.PolicyTypeStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.PolicyTypeStatus
  ( PolicyTypeStatus
      ( PolicyTypeStatus',
        Enabled,
        PendingDisable,
        PendingEnable
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PolicyTypeStatus = PolicyTypeStatus' Lude.Text
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

pattern Enabled :: PolicyTypeStatus
pattern Enabled = PolicyTypeStatus' "ENABLED"

pattern PendingDisable :: PolicyTypeStatus
pattern PendingDisable = PolicyTypeStatus' "PENDING_DISABLE"

pattern PendingEnable :: PolicyTypeStatus
pattern PendingEnable = PolicyTypeStatus' "PENDING_ENABLE"

{-# COMPLETE
  Enabled,
  PendingDisable,
  PendingEnable,
  PolicyTypeStatus'
  #-}
