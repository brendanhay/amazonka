{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.QueryType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.QueryType where

import Network.AWS.Prelude

data QueryType
  = CloudformationStack10
  | TagFilters10
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

instance FromText QueryType where
  parser =
    takeLowerText >>= \case
      "cloudformation_stack_1_0" -> pure CloudformationStack10
      "tag_filters_1_0" -> pure TagFilters10
      e ->
        fromTextError $
          "Failure parsing QueryType from value: '" <> e
            <> "'. Accepted values: cloudformation_stack_1_0, tag_filters_1_0"

instance ToText QueryType where
  toText = \case
    CloudformationStack10 -> "CLOUDFORMATION_STACK_1_0"
    TagFilters10 -> "TAG_FILTERS_1_0"

instance Hashable QueryType

instance NFData QueryType

instance ToByteString QueryType

instance ToQuery QueryType

instance ToHeader QueryType

instance ToJSON QueryType where
  toJSON = toJSONText

instance FromJSON QueryType where
  parseJSON = parseJSONText "QueryType"
