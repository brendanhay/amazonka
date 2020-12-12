{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProtectiveEquipmentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProtectiveEquipmentType
  ( ProtectiveEquipmentType
      ( ProtectiveEquipmentType',
        FaceCover,
        HandCover,
        HeadCover
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ProtectiveEquipmentType = ProtectiveEquipmentType' Lude.Text
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

pattern FaceCover :: ProtectiveEquipmentType
pattern FaceCover = ProtectiveEquipmentType' "FACE_COVER"

pattern HandCover :: ProtectiveEquipmentType
pattern HandCover = ProtectiveEquipmentType' "HAND_COVER"

pattern HeadCover :: ProtectiveEquipmentType
pattern HeadCover = ProtectiveEquipmentType' "HEAD_COVER"

{-# COMPLETE
  FaceCover,
  HandCover,
  HeadCover,
  ProtectiveEquipmentType'
  #-}
