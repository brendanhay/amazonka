{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyScopeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyScopeType where

import Network.AWS.Prelude

data PolicyScopeType
  = AWS
  | All
  | Local
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

instance FromText PolicyScopeType where
  parser =
    takeLowerText >>= \case
      "aws" -> pure AWS
      "all" -> pure All
      "local" -> pure Local
      e ->
        fromTextError $
          "Failure parsing PolicyScopeType from value: '" <> e
            <> "'. Accepted values: aws, all, local"

instance ToText PolicyScopeType where
  toText = \case
    AWS -> "AWS"
    All -> "All"
    Local -> "Local"

instance Hashable PolicyScopeType

instance NFData PolicyScopeType

instance ToByteString PolicyScopeType

instance ToQuery PolicyScopeType

instance ToHeader PolicyScopeType
