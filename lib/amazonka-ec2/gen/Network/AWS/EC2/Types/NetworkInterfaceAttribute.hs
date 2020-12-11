-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceAttribute
  ( NetworkInterfaceAttribute
      ( NetworkInterfaceAttribute',
        NIAAttachment,
        NIADescription,
        NIAGroupSet,
        NIASourceDestCheck
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype NetworkInterfaceAttribute = NetworkInterfaceAttribute' Lude.Text
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

pattern NIAAttachment :: NetworkInterfaceAttribute
pattern NIAAttachment = NetworkInterfaceAttribute' "attachment"

pattern NIADescription :: NetworkInterfaceAttribute
pattern NIADescription = NetworkInterfaceAttribute' "description"

pattern NIAGroupSet :: NetworkInterfaceAttribute
pattern NIAGroupSet = NetworkInterfaceAttribute' "groupSet"

pattern NIASourceDestCheck :: NetworkInterfaceAttribute
pattern NIASourceDestCheck = NetworkInterfaceAttribute' "sourceDestCheck"

{-# COMPLETE
  NIAAttachment,
  NIADescription,
  NIAGroupSet,
  NIASourceDestCheck,
  NetworkInterfaceAttribute'
  #-}
