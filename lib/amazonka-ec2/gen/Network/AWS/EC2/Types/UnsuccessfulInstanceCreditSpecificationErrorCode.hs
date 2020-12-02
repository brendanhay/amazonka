{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationErrorCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data UnsuccessfulInstanceCreditSpecificationErrorCode
  = IncorrectInstanceState
  | InstanceCreditSpecification_NotSupported
  | InvalidInstanceId_Malformed
  | InvalidInstanceId_NotFound
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

instance FromText UnsuccessfulInstanceCreditSpecificationErrorCode where
  parser =
    takeLowerText >>= \case
      "incorrectinstancestate" -> pure IncorrectInstanceState
      "instancecreditspecification.notsupported" -> pure InstanceCreditSpecification_NotSupported
      "invalidinstanceid.malformed" -> pure InvalidInstanceId_Malformed
      "invalidinstanceid.notfound" -> pure InvalidInstanceId_NotFound
      e ->
        fromTextError $
          "Failure parsing UnsuccessfulInstanceCreditSpecificationErrorCode from value: '" <> e
            <> "'. Accepted values: incorrectinstancestate, instancecreditspecification.notsupported, invalidinstanceid.malformed, invalidinstanceid.notfound"

instance ToText UnsuccessfulInstanceCreditSpecificationErrorCode where
  toText = \case
    IncorrectInstanceState -> "IncorrectInstanceState"
    InstanceCreditSpecification_NotSupported -> "InstanceCreditSpecification.NotSupported"
    InvalidInstanceId_Malformed -> "InvalidInstanceID.Malformed"
    InvalidInstanceId_NotFound -> "InvalidInstanceID.NotFound"

instance Hashable UnsuccessfulInstanceCreditSpecificationErrorCode

instance NFData UnsuccessfulInstanceCreditSpecificationErrorCode

instance ToByteString UnsuccessfulInstanceCreditSpecificationErrorCode

instance ToQuery UnsuccessfulInstanceCreditSpecificationErrorCode

instance ToHeader UnsuccessfulInstanceCreditSpecificationErrorCode

instance FromXML UnsuccessfulInstanceCreditSpecificationErrorCode where
  parseXML = parseXMLText "UnsuccessfulInstanceCreditSpecificationErrorCode"
