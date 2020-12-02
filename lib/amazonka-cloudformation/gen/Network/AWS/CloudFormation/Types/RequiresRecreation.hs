{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.RequiresRecreation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.RequiresRecreation where

import Network.AWS.Prelude

data RequiresRecreation
  = Always
  | Conditionally
  | Never
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

instance FromText RequiresRecreation where
  parser =
    takeLowerText >>= \case
      "always" -> pure Always
      "conditionally" -> pure Conditionally
      "never" -> pure Never
      e ->
        fromTextError $
          "Failure parsing RequiresRecreation from value: '" <> e
            <> "'. Accepted values: always, conditionally, never"

instance ToText RequiresRecreation where
  toText = \case
    Always -> "Always"
    Conditionally -> "Conditionally"
    Never -> "Never"

instance Hashable RequiresRecreation

instance NFData RequiresRecreation

instance ToByteString RequiresRecreation

instance ToQuery RequiresRecreation

instance ToHeader RequiresRecreation

instance FromXML RequiresRecreation where
  parseXML = parseXMLText "RequiresRecreation"
