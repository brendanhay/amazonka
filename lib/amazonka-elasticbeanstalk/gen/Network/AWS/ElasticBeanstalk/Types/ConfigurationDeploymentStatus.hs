{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ConfigurationDeploymentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ConfigurationDeploymentStatus where

import Network.AWS.Prelude

data ConfigurationDeploymentStatus
  = CDSDeployed
  | CDSFailed
  | CDSPending
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

instance FromText ConfigurationDeploymentStatus where
  parser =
    takeLowerText >>= \case
      "deployed" -> pure CDSDeployed
      "failed" -> pure CDSFailed
      "pending" -> pure CDSPending
      e ->
        fromTextError $
          "Failure parsing ConfigurationDeploymentStatus from value: '" <> e
            <> "'. Accepted values: deployed, failed, pending"

instance ToText ConfigurationDeploymentStatus where
  toText = \case
    CDSDeployed -> "deployed"
    CDSFailed -> "failed"
    CDSPending -> "pending"

instance Hashable ConfigurationDeploymentStatus

instance NFData ConfigurationDeploymentStatus

instance ToByteString ConfigurationDeploymentStatus

instance ToQuery ConfigurationDeploymentStatus

instance ToHeader ConfigurationDeploymentStatus

instance FromXML ConfigurationDeploymentStatus where
  parseXML = parseXMLText "ConfigurationDeploymentStatus"
