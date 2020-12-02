{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Action
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.Action where

import Network.AWS.Prelude

data Action
  = ClipboardCopyFromLocalDevice
  | ClipboardCopyToLocalDevice
  | FileDownload
  | FileUpload
  | PrintingToLocalDevice
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

instance FromText Action where
  parser =
    takeLowerText >>= \case
      "clipboard_copy_from_local_device" -> pure ClipboardCopyFromLocalDevice
      "clipboard_copy_to_local_device" -> pure ClipboardCopyToLocalDevice
      "file_download" -> pure FileDownload
      "file_upload" -> pure FileUpload
      "printing_to_local_device" -> pure PrintingToLocalDevice
      e ->
        fromTextError $
          "Failure parsing Action from value: '" <> e
            <> "'. Accepted values: clipboard_copy_from_local_device, clipboard_copy_to_local_device, file_download, file_upload, printing_to_local_device"

instance ToText Action where
  toText = \case
    ClipboardCopyFromLocalDevice -> "CLIPBOARD_COPY_FROM_LOCAL_DEVICE"
    ClipboardCopyToLocalDevice -> "CLIPBOARD_COPY_TO_LOCAL_DEVICE"
    FileDownload -> "FILE_DOWNLOAD"
    FileUpload -> "FILE_UPLOAD"
    PrintingToLocalDevice -> "PRINTING_TO_LOCAL_DEVICE"

instance Hashable Action

instance NFData Action

instance ToByteString Action

instance ToQuery Action

instance ToHeader Action

instance ToJSON Action where
  toJSON = toJSONText

instance FromJSON Action where
  parseJSON = parseJSONText "Action"
