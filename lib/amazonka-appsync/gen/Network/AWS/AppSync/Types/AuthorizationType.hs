{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.AuthorizationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.AuthorizationType where

import Network.AWS.Prelude

data AuthorizationType = AWSIAM
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

instance FromText AuthorizationType where
  parser =
    takeLowerText >>= \case
      "aws_iam" -> pure AWSIAM
      e ->
        fromTextError $
          "Failure parsing AuthorizationType from value: '" <> e
            <> "'. Accepted values: aws_iam"

instance ToText AuthorizationType where
  toText = \case
    AWSIAM -> "AWS_IAM"

instance Hashable AuthorizationType

instance NFData AuthorizationType

instance ToByteString AuthorizationType

instance ToQuery AuthorizationType

instance ToHeader AuthorizationType

instance ToJSON AuthorizationType where
  toJSON = toJSONText

instance FromJSON AuthorizationType where
  parseJSON = parseJSONText "AuthorizationType"
