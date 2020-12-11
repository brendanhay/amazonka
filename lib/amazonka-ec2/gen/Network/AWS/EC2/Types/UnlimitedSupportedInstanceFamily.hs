-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UnlimitedSupportedInstanceFamily
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnlimitedSupportedInstanceFamily
  ( UnlimitedSupportedInstanceFamily
      ( UnlimitedSupportedInstanceFamily',
        T2,
        T3,
        T3a,
        T4g
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype UnlimitedSupportedInstanceFamily = UnlimitedSupportedInstanceFamily' Lude.Text
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

pattern T2 :: UnlimitedSupportedInstanceFamily
pattern T2 = UnlimitedSupportedInstanceFamily' "t2"

pattern T3 :: UnlimitedSupportedInstanceFamily
pattern T3 = UnlimitedSupportedInstanceFamily' "t3"

pattern T3a :: UnlimitedSupportedInstanceFamily
pattern T3a = UnlimitedSupportedInstanceFamily' "t3a"

pattern T4g :: UnlimitedSupportedInstanceFamily
pattern T4g = UnlimitedSupportedInstanceFamily' "t4g"

{-# COMPLETE
  T2,
  T3,
  T3a,
  T4g,
  UnlimitedSupportedInstanceFamily'
  #-}
