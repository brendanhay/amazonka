{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.FailureType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.FailureType where

import Network.AWS.Prelude

data FailureType
  = ConfigurationError
  | JobFailed
  | PermissionError
  | RevisionOutOfSync
  | RevisionUnavailable
  | SystemUnavailable
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

instance FromText FailureType where
  parser =
    takeLowerText >>= \case
      "configurationerror" -> pure ConfigurationError
      "jobfailed" -> pure JobFailed
      "permissionerror" -> pure PermissionError
      "revisionoutofsync" -> pure RevisionOutOfSync
      "revisionunavailable" -> pure RevisionUnavailable
      "systemunavailable" -> pure SystemUnavailable
      e ->
        fromTextError $
          "Failure parsing FailureType from value: '" <> e
            <> "'. Accepted values: configurationerror, jobfailed, permissionerror, revisionoutofsync, revisionunavailable, systemunavailable"

instance ToText FailureType where
  toText = \case
    ConfigurationError -> "ConfigurationError"
    JobFailed -> "JobFailed"
    PermissionError -> "PermissionError"
    RevisionOutOfSync -> "RevisionOutOfSync"
    RevisionUnavailable -> "RevisionUnavailable"
    SystemUnavailable -> "SystemUnavailable"

instance Hashable FailureType

instance NFData FailureType

instance ToByteString FailureType

instance ToQuery FailureType

instance ToHeader FailureType

instance ToJSON FailureType where
  toJSON = toJSONText
