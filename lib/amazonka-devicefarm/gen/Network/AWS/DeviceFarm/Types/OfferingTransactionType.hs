-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.OfferingTransactionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.OfferingTransactionType
  ( OfferingTransactionType
      ( OfferingTransactionType',
        Purchase,
        Renew,
        System
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OfferingTransactionType = OfferingTransactionType' Lude.Text
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

pattern Purchase :: OfferingTransactionType
pattern Purchase = OfferingTransactionType' "PURCHASE"

pattern Renew :: OfferingTransactionType
pattern Renew = OfferingTransactionType' "RENEW"

pattern System :: OfferingTransactionType
pattern System = OfferingTransactionType' "SYSTEM"

{-# COMPLETE
  Purchase,
  Renew,
  System,
  OfferingTransactionType'
  #-}
