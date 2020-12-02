{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.AppAttributesKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.AppAttributesKeys where

import Network.AWS.Prelude

data AppAttributesKeys
  = AWSFlowRubySettings
  | AutoBundleOnDeploy
  | DocumentRoot
  | RailsEnv
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

instance FromText AppAttributesKeys where
  parser =
    takeLowerText >>= \case
      "awsflowrubysettings" -> pure AWSFlowRubySettings
      "autobundleondeploy" -> pure AutoBundleOnDeploy
      "documentroot" -> pure DocumentRoot
      "railsenv" -> pure RailsEnv
      e ->
        fromTextError $
          "Failure parsing AppAttributesKeys from value: '" <> e
            <> "'. Accepted values: awsflowrubysettings, autobundleondeploy, documentroot, railsenv"

instance ToText AppAttributesKeys where
  toText = \case
    AWSFlowRubySettings -> "AwsFlowRubySettings"
    AutoBundleOnDeploy -> "AutoBundleOnDeploy"
    DocumentRoot -> "DocumentRoot"
    RailsEnv -> "RailsEnv"

instance Hashable AppAttributesKeys

instance NFData AppAttributesKeys

instance ToByteString AppAttributesKeys

instance ToQuery AppAttributesKeys

instance ToHeader AppAttributesKeys

instance ToJSON AppAttributesKeys where
  toJSON = toJSONText

instance FromJSON AppAttributesKeys where
  parseJSON = parseJSONText "AppAttributesKeys"
