{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AttachmentsSourceKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AttachmentsSourceKey where

import Network.AWS.Prelude

data AttachmentsSourceKey
  = AttachmentReference
  | S3FileURL
  | SourceURL
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

instance FromText AttachmentsSourceKey where
  parser =
    takeLowerText >>= \case
      "attachmentreference" -> pure AttachmentReference
      "s3fileurl" -> pure S3FileURL
      "sourceurl" -> pure SourceURL
      e ->
        fromTextError $
          "Failure parsing AttachmentsSourceKey from value: '" <> e
            <> "'. Accepted values: attachmentreference, s3fileurl, sourceurl"

instance ToText AttachmentsSourceKey where
  toText = \case
    AttachmentReference -> "AttachmentReference"
    S3FileURL -> "S3FileUrl"
    SourceURL -> "SourceUrl"

instance Hashable AttachmentsSourceKey

instance NFData AttachmentsSourceKey

instance ToByteString AttachmentsSourceKey

instance ToQuery AttachmentsSourceKey

instance ToHeader AttachmentsSourceKey

instance ToJSON AttachmentsSourceKey where
  toJSON = toJSONText
