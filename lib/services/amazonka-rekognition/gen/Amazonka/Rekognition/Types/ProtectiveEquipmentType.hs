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
-- Module      : Amazonka.Rekognition.Types.ProtectiveEquipmentType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.ProtectiveEquipmentType
  ( ProtectiveEquipmentType
      ( ..,
        ProtectiveEquipmentType_FACE_COVER,
        ProtectiveEquipmentType_HAND_COVER,
        ProtectiveEquipmentType_HEAD_COVER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProtectiveEquipmentType = ProtectiveEquipmentType'
  { fromProtectiveEquipmentType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
