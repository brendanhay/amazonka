-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.MulticastSupportValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.MulticastSupportValue
  ( MulticastSupportValue
      ( MulticastSupportValue',
        MSVDisable,
        MSVEnable
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MulticastSupportValue = MulticastSupportValue' Lude.Text
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

pattern MSVDisable :: MulticastSupportValue
pattern MSVDisable = MulticastSupportValue' "disable"

pattern MSVEnable :: MulticastSupportValue
pattern MSVEnable = MulticastSupportValue' "enable"

{-# COMPLETE
  MSVDisable,
  MSVEnable,
  MulticastSupportValue'
  #-}
