{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AccelerationMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AccelerationMode where

import Network.AWS.Prelude

-- | Specify whether the service runs your job with accelerated transcoding. Choose DISABLED if you don't want accelerated transcoding. Choose ENABLED if you want your job to run with accelerated transcoding and to fail if your input files or your job settings aren't compatible with accelerated transcoding. Choose PREFERRED if you want your job to run with accelerated transcoding if the job is compatible with the feature and to run at standard speed if it's not.
data AccelerationMode
  = AMDisabled
  | AMEnabled
  | AMPreferred
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

instance FromText AccelerationMode where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure AMDisabled
      "enabled" -> pure AMEnabled
      "preferred" -> pure AMPreferred
      e ->
        fromTextError $
          "Failure parsing AccelerationMode from value: '" <> e
            <> "'. Accepted values: disabled, enabled, preferred"

instance ToText AccelerationMode where
  toText = \case
    AMDisabled -> "DISABLED"
    AMEnabled -> "ENABLED"
    AMPreferred -> "PREFERRED"

instance Hashable AccelerationMode

instance NFData AccelerationMode

instance ToByteString AccelerationMode

instance ToQuery AccelerationMode

instance ToHeader AccelerationMode

instance ToJSON AccelerationMode where
  toJSON = toJSONText

instance FromJSON AccelerationMode where
  parseJSON = parseJSONText "AccelerationMode"
