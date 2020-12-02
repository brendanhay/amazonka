{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PermissionsBoundaryAttachmentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PermissionsBoundaryAttachmentType where

import Network.AWS.Prelude

data PermissionsBoundaryAttachmentType = PermissionsBoundaryPolicy
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

instance FromText PermissionsBoundaryAttachmentType where
  parser =
    takeLowerText >>= \case
      "permissionsboundarypolicy" -> pure PermissionsBoundaryPolicy
      e ->
        fromTextError $
          "Failure parsing PermissionsBoundaryAttachmentType from value: '" <> e
            <> "'. Accepted values: permissionsboundarypolicy"

instance ToText PermissionsBoundaryAttachmentType where
  toText = \case
    PermissionsBoundaryPolicy -> "PermissionsBoundaryPolicy"

instance Hashable PermissionsBoundaryAttachmentType

instance NFData PermissionsBoundaryAttachmentType

instance ToByteString PermissionsBoundaryAttachmentType

instance ToQuery PermissionsBoundaryAttachmentType

instance ToHeader PermissionsBoundaryAttachmentType

instance FromXML PermissionsBoundaryAttachmentType where
  parseXML = parseXMLText "PermissionsBoundaryAttachmentType"
