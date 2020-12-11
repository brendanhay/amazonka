-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IPv6SupportValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IPv6SupportValue
  ( IPv6SupportValue
      ( IPv6SupportValue',
        ISVDisable,
        ISVEnable
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype IPv6SupportValue = IPv6SupportValue' Lude.Text
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

pattern ISVDisable :: IPv6SupportValue
pattern ISVDisable = IPv6SupportValue' "disable"

pattern ISVEnable :: IPv6SupportValue
pattern ISVEnable = IPv6SupportValue' "enable"

{-# COMPLETE
  ISVDisable,
  ISVEnable,
  IPv6SupportValue'
  #-}
