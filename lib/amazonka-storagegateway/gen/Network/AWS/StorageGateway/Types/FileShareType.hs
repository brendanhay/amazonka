{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.FileShareType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.FileShareType where

import Network.AWS.Prelude

-- | The type of the file share.
data FileShareType
  = Nfs
  | Smb
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

instance FromText FileShareType where
  parser =
    takeLowerText >>= \case
      "nfs" -> pure Nfs
      "smb" -> pure Smb
      e ->
        fromTextError $
          "Failure parsing FileShareType from value: '" <> e
            <> "'. Accepted values: nfs, smb"

instance ToText FileShareType where
  toText = \case
    Nfs -> "NFS"
    Smb -> "SMB"

instance Hashable FileShareType

instance NFData FileShareType

instance ToByteString FileShareType

instance ToQuery FileShareType

instance ToHeader FileShareType

instance FromJSON FileShareType where
  parseJSON = parseJSONText "FileShareType"
