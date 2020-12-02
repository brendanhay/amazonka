{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AuthMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AuthMode where

import Network.AWS.Prelude

data AuthMode
  = IAM
  | SSO
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

instance FromText AuthMode where
  parser =
    takeLowerText >>= \case
      "iam" -> pure IAM
      "sso" -> pure SSO
      e ->
        fromTextError $
          "Failure parsing AuthMode from value: '" <> e
            <> "'. Accepted values: iam, sso"

instance ToText AuthMode where
  toText = \case
    IAM -> "IAM"
    SSO -> "SSO"

instance Hashable AuthMode

instance NFData AuthMode

instance ToByteString AuthMode

instance ToQuery AuthMode

instance ToHeader AuthMode

instance ToJSON AuthMode where
  toJSON = toJSONText

instance FromJSON AuthMode where
  parseJSON = parseJSONText "AuthMode"
