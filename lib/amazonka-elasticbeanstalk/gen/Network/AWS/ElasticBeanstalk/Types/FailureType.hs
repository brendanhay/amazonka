{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.FailureType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.FailureType where

import Network.AWS.Prelude

data FailureType
  = CancellationFailed
  | InternalFailure
  | InvalidEnvironmentState
  | PermissionsError
  | RollbackFailed
  | RollbackSuccessful
  | UpdateCancelled
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
      "cancellationfailed" -> pure CancellationFailed
      "internalfailure" -> pure InternalFailure
      "invalidenvironmentstate" -> pure InvalidEnvironmentState
      "permissionserror" -> pure PermissionsError
      "rollbackfailed" -> pure RollbackFailed
      "rollbacksuccessful" -> pure RollbackSuccessful
      "updatecancelled" -> pure UpdateCancelled
      e ->
        fromTextError $
          "Failure parsing FailureType from value: '" <> e
            <> "'. Accepted values: cancellationfailed, internalfailure, invalidenvironmentstate, permissionserror, rollbackfailed, rollbacksuccessful, updatecancelled"

instance ToText FailureType where
  toText = \case
    CancellationFailed -> "CancellationFailed"
    InternalFailure -> "InternalFailure"
    InvalidEnvironmentState -> "InvalidEnvironmentState"
    PermissionsError -> "PermissionsError"
    RollbackFailed -> "RollbackFailed"
    RollbackSuccessful -> "RollbackSuccessful"
    UpdateCancelled -> "UpdateCancelled"

instance Hashable FailureType

instance NFData FailureType

instance ToByteString FailureType

instance ToQuery FailureType

instance ToHeader FailureType

instance FromXML FailureType where
  parseXML = parseXMLText "FailureType"
