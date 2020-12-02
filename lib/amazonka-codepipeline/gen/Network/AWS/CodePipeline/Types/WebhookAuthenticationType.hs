{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.WebhookAuthenticationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.WebhookAuthenticationType where

import Network.AWS.Prelude

data WebhookAuthenticationType
  = GithubHmac
  | IP
  | Unauthenticated
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

instance FromText WebhookAuthenticationType where
  parser =
    takeLowerText >>= \case
      "github_hmac" -> pure GithubHmac
      "ip" -> pure IP
      "unauthenticated" -> pure Unauthenticated
      e ->
        fromTextError $
          "Failure parsing WebhookAuthenticationType from value: '" <> e
            <> "'. Accepted values: github_hmac, ip, unauthenticated"

instance ToText WebhookAuthenticationType where
  toText = \case
    GithubHmac -> "GITHUB_HMAC"
    IP -> "IP"
    Unauthenticated -> "UNAUTHENTICATED"

instance Hashable WebhookAuthenticationType

instance NFData WebhookAuthenticationType

instance ToByteString WebhookAuthenticationType

instance ToQuery WebhookAuthenticationType

instance ToHeader WebhookAuthenticationType

instance ToJSON WebhookAuthenticationType where
  toJSON = toJSONText

instance FromJSON WebhookAuthenticationType where
  parseJSON = parseJSONText "WebhookAuthenticationType"
