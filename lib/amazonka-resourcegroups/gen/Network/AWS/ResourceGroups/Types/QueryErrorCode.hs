{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.QueryErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.QueryErrorCode where

import Network.AWS.Prelude

data QueryErrorCode
  = CloudformationStackInactive
  | CloudformationStackNotExisting
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

instance FromText QueryErrorCode where
  parser =
    takeLowerText >>= \case
      "cloudformation_stack_inactive" -> pure CloudformationStackInactive
      "cloudformation_stack_not_existing" -> pure CloudformationStackNotExisting
      e ->
        fromTextError $
          "Failure parsing QueryErrorCode from value: '" <> e
            <> "'. Accepted values: cloudformation_stack_inactive, cloudformation_stack_not_existing"

instance ToText QueryErrorCode where
  toText = \case
    CloudformationStackInactive -> "CLOUDFORMATION_STACK_INACTIVE"
    CloudformationStackNotExisting -> "CLOUDFORMATION_STACK_NOT_EXISTING"

instance Hashable QueryErrorCode

instance NFData QueryErrorCode

instance ToByteString QueryErrorCode

instance ToQuery QueryErrorCode

instance ToHeader QueryErrorCode

instance FromJSON QueryErrorCode where
  parseJSON = parseJSONText "QueryErrorCode"
