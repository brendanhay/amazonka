{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.EFSAuthorizationConfigIAM
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.EFSAuthorizationConfigIAM where

import Network.AWS.Prelude

data EFSAuthorizationConfigIAM
  = EFSACIDisabled
  | EFSACIEnabled
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

instance FromText EFSAuthorizationConfigIAM where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure EFSACIDisabled
      "enabled" -> pure EFSACIEnabled
      e ->
        fromTextError $
          "Failure parsing EFSAuthorizationConfigIAM from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText EFSAuthorizationConfigIAM where
  toText = \case
    EFSACIDisabled -> "DISABLED"
    EFSACIEnabled -> "ENABLED"

instance Hashable EFSAuthorizationConfigIAM

instance NFData EFSAuthorizationConfigIAM

instance ToByteString EFSAuthorizationConfigIAM

instance ToQuery EFSAuthorizationConfigIAM

instance ToHeader EFSAuthorizationConfigIAM

instance ToJSON EFSAuthorizationConfigIAM where
  toJSON = toJSONText

instance FromJSON EFSAuthorizationConfigIAM where
  parseJSON = parseJSONText "EFSAuthorizationConfigIAM"
