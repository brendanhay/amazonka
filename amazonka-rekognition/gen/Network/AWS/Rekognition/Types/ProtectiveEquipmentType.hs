{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProtectiveEquipmentType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProtectiveEquipmentType
  ( ProtectiveEquipmentType
      ( ..,
        ProtectiveEquipmentType_FACE_COVER,
        ProtectiveEquipmentType_HAND_COVER,
        ProtectiveEquipmentType_HEAD_COVER
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ProtectiveEquipmentType = ProtectiveEquipmentType'
  { fromProtectiveEquipmentType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ProtectiveEquipmentType_FACE_COVER :: ProtectiveEquipmentType
pattern ProtectiveEquipmentType_FACE_COVER = ProtectiveEquipmentType' "FACE_COVER"

pattern ProtectiveEquipmentType_HAND_COVER :: ProtectiveEquipmentType
pattern ProtectiveEquipmentType_HAND_COVER = ProtectiveEquipmentType' "HAND_COVER"

pattern ProtectiveEquipmentType_HEAD_COVER :: ProtectiveEquipmentType
pattern ProtectiveEquipmentType_HEAD_COVER = ProtectiveEquipmentType' "HEAD_COVER"

{-# COMPLETE
  ProtectiveEquipmentType_FACE_COVER,
  ProtectiveEquipmentType_HAND_COVER,
  ProtectiveEquipmentType_HEAD_COVER,
  ProtectiveEquipmentType'
  #-}
