{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateErrorCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data LaunchTemplateErrorCode
  = LaunchTemplateIdDoesNotExist
  | LaunchTemplateIdMalformed
  | LaunchTemplateNameDoesNotExist
  | LaunchTemplateNameMalformed
  | LaunchTemplateVersionDoesNotExist
  | UnexpectedError
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

instance FromText LaunchTemplateErrorCode where
  parser =
    takeLowerText >>= \case
      "launchtemplateiddoesnotexist" -> pure LaunchTemplateIdDoesNotExist
      "launchtemplateidmalformed" -> pure LaunchTemplateIdMalformed
      "launchtemplatenamedoesnotexist" -> pure LaunchTemplateNameDoesNotExist
      "launchtemplatenamemalformed" -> pure LaunchTemplateNameMalformed
      "launchtemplateversiondoesnotexist" -> pure LaunchTemplateVersionDoesNotExist
      "unexpectederror" -> pure UnexpectedError
      e ->
        fromTextError $
          "Failure parsing LaunchTemplateErrorCode from value: '" <> e
            <> "'. Accepted values: launchtemplateiddoesnotexist, launchtemplateidmalformed, launchtemplatenamedoesnotexist, launchtemplatenamemalformed, launchtemplateversiondoesnotexist, unexpectederror"

instance ToText LaunchTemplateErrorCode where
  toText = \case
    LaunchTemplateIdDoesNotExist -> "launchTemplateIdDoesNotExist"
    LaunchTemplateIdMalformed -> "launchTemplateIdMalformed"
    LaunchTemplateNameDoesNotExist -> "launchTemplateNameDoesNotExist"
    LaunchTemplateNameMalformed -> "launchTemplateNameMalformed"
    LaunchTemplateVersionDoesNotExist -> "launchTemplateVersionDoesNotExist"
    UnexpectedError -> "unexpectedError"

instance Hashable LaunchTemplateErrorCode

instance NFData LaunchTemplateErrorCode

instance ToByteString LaunchTemplateErrorCode

instance ToQuery LaunchTemplateErrorCode

instance ToHeader LaunchTemplateErrorCode

instance FromXML LaunchTemplateErrorCode where
  parseXML = parseXMLText "LaunchTemplateErrorCode"
