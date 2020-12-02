{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.DependentServiceName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.DependentServiceName where

import Network.AWS.Prelude

data DependentServiceName
  = AWSSHieldAdvanced
  | AWSconfig
  | AWSvpc
  | AWSwaf
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

instance FromText DependentServiceName where
  parser =
    takeLowerText >>= \case
      "awsshield_advanced" -> pure AWSSHieldAdvanced
      "awsconfig" -> pure AWSconfig
      "awsvpc" -> pure AWSvpc
      "awswaf" -> pure AWSwaf
      e ->
        fromTextError $
          "Failure parsing DependentServiceName from value: '" <> e
            <> "'. Accepted values: awsshield_advanced, awsconfig, awsvpc, awswaf"

instance ToText DependentServiceName where
  toText = \case
    AWSSHieldAdvanced -> "AWSSHIELD_ADVANCED"
    AWSconfig -> "AWSCONFIG"
    AWSvpc -> "AWSVPC"
    AWSwaf -> "AWSWAF"

instance Hashable DependentServiceName

instance NFData DependentServiceName

instance ToByteString DependentServiceName

instance ToQuery DependentServiceName

instance ToHeader DependentServiceName

instance FromJSON DependentServiceName where
  parseJSON = parseJSONText "DependentServiceName"
