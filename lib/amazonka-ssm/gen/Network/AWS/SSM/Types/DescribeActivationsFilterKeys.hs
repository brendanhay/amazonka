{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DescribeActivationsFilterKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DescribeActivationsFilterKeys where

import Network.AWS.Prelude

data DescribeActivationsFilterKeys
  = DAFKActivationIds
  | DAFKDefaultInstanceName
  | DAFKIAMRole
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

instance FromText DescribeActivationsFilterKeys where
  parser =
    takeLowerText >>= \case
      "activationids" -> pure DAFKActivationIds
      "defaultinstancename" -> pure DAFKDefaultInstanceName
      "iamrole" -> pure DAFKIAMRole
      e ->
        fromTextError $
          "Failure parsing DescribeActivationsFilterKeys from value: '" <> e
            <> "'. Accepted values: activationids, defaultinstancename, iamrole"

instance ToText DescribeActivationsFilterKeys where
  toText = \case
    DAFKActivationIds -> "ActivationIds"
    DAFKDefaultInstanceName -> "DefaultInstanceName"
    DAFKIAMRole -> "IamRole"

instance Hashable DescribeActivationsFilterKeys

instance NFData DescribeActivationsFilterKeys

instance ToByteString DescribeActivationsFilterKeys

instance ToQuery DescribeActivationsFilterKeys

instance ToHeader DescribeActivationsFilterKeys

instance ToJSON DescribeActivationsFilterKeys where
  toJSON = toJSONText
