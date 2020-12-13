{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeType
  ( VolumeType
      ( VolumeType',
        Standard,
        IO1,
        IO2,
        GP2,
        SC1,
        ST1
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype VolumeType = VolumeType' Lude.Text
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

pattern Standard :: VolumeType
pattern Standard = VolumeType' "standard"

pattern IO1 :: VolumeType
pattern IO1 = VolumeType' "io1"

pattern IO2 :: VolumeType
pattern IO2 = VolumeType' "io2"

pattern GP2 :: VolumeType
pattern GP2 = VolumeType' "gp2"

pattern SC1 :: VolumeType
pattern SC1 = VolumeType' "sc1"

pattern ST1 :: VolumeType
pattern ST1 = VolumeType' "st1"

{-# COMPLETE
  Standard,
  IO1,
  IO2,
  GP2,
  SC1,
  ST1,
  VolumeType'
  #-}
