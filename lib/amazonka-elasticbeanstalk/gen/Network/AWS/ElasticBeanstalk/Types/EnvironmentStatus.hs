{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentStatus where

import Network.AWS.Prelude

data EnvironmentStatus
  = ESAborting
  | ESLaunching
  | ESLinkingFrom
  | ESLinkingTo
  | ESReady
  | ESTerminated
  | ESTerminating
  | ESUpdating
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

instance FromText EnvironmentStatus where
  parser =
    takeLowerText >>= \case
      "aborting" -> pure ESAborting
      "launching" -> pure ESLaunching
      "linkingfrom" -> pure ESLinkingFrom
      "linkingto" -> pure ESLinkingTo
      "ready" -> pure ESReady
      "terminated" -> pure ESTerminated
      "terminating" -> pure ESTerminating
      "updating" -> pure ESUpdating
      e ->
        fromTextError $
          "Failure parsing EnvironmentStatus from value: '" <> e
            <> "'. Accepted values: aborting, launching, linkingfrom, linkingto, ready, terminated, terminating, updating"

instance ToText EnvironmentStatus where
  toText = \case
    ESAborting -> "Aborting"
    ESLaunching -> "Launching"
    ESLinkingFrom -> "LinkingFrom"
    ESLinkingTo -> "LinkingTo"
    ESReady -> "Ready"
    ESTerminated -> "Terminated"
    ESTerminating -> "Terminating"
    ESUpdating -> "Updating"

instance Hashable EnvironmentStatus

instance NFData EnvironmentStatus

instance ToByteString EnvironmentStatus

instance ToQuery EnvironmentStatus

instance ToHeader EnvironmentStatus

instance FromXML EnvironmentStatus where
  parseXML = parseXMLText "EnvironmentStatus"
