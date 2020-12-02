{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImageStateChangeReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImageStateChangeReasonCode where

import Network.AWS.Prelude

data ImageStateChangeReasonCode
  = ISCRCImageBuilderNotAvailable
  | ISCRCImageCopyFailure
  | ISCRCInternalError
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

instance FromText ImageStateChangeReasonCode where
  parser =
    takeLowerText >>= \case
      "image_builder_not_available" -> pure ISCRCImageBuilderNotAvailable
      "image_copy_failure" -> pure ISCRCImageCopyFailure
      "internal_error" -> pure ISCRCInternalError
      e ->
        fromTextError $
          "Failure parsing ImageStateChangeReasonCode from value: '" <> e
            <> "'. Accepted values: image_builder_not_available, image_copy_failure, internal_error"

instance ToText ImageStateChangeReasonCode where
  toText = \case
    ISCRCImageBuilderNotAvailable -> "IMAGE_BUILDER_NOT_AVAILABLE"
    ISCRCImageCopyFailure -> "IMAGE_COPY_FAILURE"
    ISCRCInternalError -> "INTERNAL_ERROR"

instance Hashable ImageStateChangeReasonCode

instance NFData ImageStateChangeReasonCode

instance ToByteString ImageStateChangeReasonCode

instance ToQuery ImageStateChangeReasonCode

instance ToHeader ImageStateChangeReasonCode

instance FromJSON ImageStateChangeReasonCode where
  parseJSON = parseJSONText "ImageStateChangeReasonCode"
