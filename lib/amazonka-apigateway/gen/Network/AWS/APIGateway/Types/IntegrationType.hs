{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.IntegrationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.IntegrationType where

import Network.AWS.Prelude

-- | The integration type. The valid value is @HTTP@ for integrating an API method with an HTTP backend; @AWS@ with any AWS service endpoints; @MOCK@ for testing without actually invoking the backend; @HTTP_PROXY@ for integrating with the HTTP proxy integration; @AWS_PROXY@ for integrating with the Lambda proxy integration.
data IntegrationType
  = AWS
  | AWSProxy
  | HTTP
  | HTTPProxy
  | Mock
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

instance FromText IntegrationType where
  parser =
    takeLowerText >>= \case
      "aws" -> pure AWS
      "aws_proxy" -> pure AWSProxy
      "http" -> pure HTTP
      "http_proxy" -> pure HTTPProxy
      "mock" -> pure Mock
      e ->
        fromTextError $
          "Failure parsing IntegrationType from value: '" <> e
            <> "'. Accepted values: aws, aws_proxy, http, http_proxy, mock"

instance ToText IntegrationType where
  toText = \case
    AWS -> "AWS"
    AWSProxy -> "AWS_PROXY"
    HTTP -> "HTTP"
    HTTPProxy -> "HTTP_PROXY"
    Mock -> "MOCK"

instance Hashable IntegrationType

instance NFData IntegrationType

instance ToByteString IntegrationType

instance ToQuery IntegrationType

instance ToHeader IntegrationType

instance ToJSON IntegrationType where
  toJSON = toJSONText

instance FromJSON IntegrationType where
  parseJSON = parseJSONText "IntegrationType"
