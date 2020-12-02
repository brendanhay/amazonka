{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImageBuilderStateChangeReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImageBuilderStateChangeReasonCode where

import Network.AWS.Prelude

data ImageBuilderStateChangeReasonCode
  = ImageUnavailable
  | InternalError
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

instance FromText ImageBuilderStateChangeReasonCode where
  parser =
    takeLowerText >>= \case
      "image_unavailable" -> pure ImageUnavailable
      "internal_error" -> pure InternalError
      e ->
        fromTextError $
          "Failure parsing ImageBuilderStateChangeReasonCode from value: '" <> e
            <> "'. Accepted values: image_unavailable, internal_error"

instance ToText ImageBuilderStateChangeReasonCode where
  toText = \case
    ImageUnavailable -> "IMAGE_UNAVAILABLE"
    InternalError -> "INTERNAL_ERROR"

instance Hashable ImageBuilderStateChangeReasonCode

instance NFData ImageBuilderStateChangeReasonCode

instance ToByteString ImageBuilderStateChangeReasonCode

instance ToQuery ImageBuilderStateChangeReasonCode

instance ToHeader ImageBuilderStateChangeReasonCode

instance FromJSON ImageBuilderStateChangeReasonCode where
  parseJSON = parseJSONText "ImageBuilderStateChangeReasonCode"
