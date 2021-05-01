{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype ProtectiveEquipmentType = ProtectiveEquipmentType'
  { fromProtectiveEquipmentType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
