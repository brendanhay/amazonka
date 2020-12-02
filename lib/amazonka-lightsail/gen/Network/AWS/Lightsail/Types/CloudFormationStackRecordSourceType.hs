{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceType where

import Network.AWS.Prelude

data CloudFormationStackRecordSourceType = ExportSnapshotRecord
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText CloudFormationStackRecordSourceType where
  parser =
    takeLowerText >>= \case
      "exportsnapshotrecord" -> pure ExportSnapshotRecord
      e ->
        fromTextError $
          "Failure parsing CloudFormationStackRecordSourceType from value: '" <> e
            <> "'. Accepted values: exportsnapshotrecord"

instance ToText CloudFormationStackRecordSourceType where
  toText = \case
    ExportSnapshotRecord -> "ExportSnapshotRecord"

instance Hashable CloudFormationStackRecordSourceType

instance NFData CloudFormationStackRecordSourceType

instance ToByteString CloudFormationStackRecordSourceType

instance ToQuery CloudFormationStackRecordSourceType

instance ToHeader CloudFormationStackRecordSourceType

instance FromJSON CloudFormationStackRecordSourceType where
  parseJSON = parseJSONText "CloudFormationStackRecordSourceType"
