-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusInfoStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusInfoStatus
  ( VolumeStatusInfoStatus
      ( VolumeStatusInfoStatus',
        Impaired,
        InsufficientData,
        OK
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype VolumeStatusInfoStatus = VolumeStatusInfoStatus' Lude.Text
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

pattern Impaired :: VolumeStatusInfoStatus
pattern Impaired = VolumeStatusInfoStatus' "impaired"

pattern InsufficientData :: VolumeStatusInfoStatus
pattern InsufficientData = VolumeStatusInfoStatus' "insufficient-data"

pattern OK :: VolumeStatusInfoStatus
pattern OK = VolumeStatusInfoStatus' "ok"

{-# COMPLETE
  Impaired,
  InsufficientData,
  OK,
  VolumeStatusInfoStatus'
  #-}
