{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.RegistrationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.RegistrationStatus where

import Network.AWS.Prelude

data RegistrationStatus
  = RSComplete
  | RSFailed
  | RSInProgress
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

instance FromText RegistrationStatus where
  parser =
    takeLowerText >>= \case
      "complete" -> pure RSComplete
      "failed" -> pure RSFailed
      "in_progress" -> pure RSInProgress
      e ->
        fromTextError $
          "Failure parsing RegistrationStatus from value: '" <> e
            <> "'. Accepted values: complete, failed, in_progress"

instance ToText RegistrationStatus where
  toText = \case
    RSComplete -> "COMPLETE"
    RSFailed -> "FAILED"
    RSInProgress -> "IN_PROGRESS"

instance Hashable RegistrationStatus

instance NFData RegistrationStatus

instance ToByteString RegistrationStatus

instance ToQuery RegistrationStatus

instance ToHeader RegistrationStatus

instance FromXML RegistrationStatus where
  parseXML = parseXMLText "RegistrationStatus"
