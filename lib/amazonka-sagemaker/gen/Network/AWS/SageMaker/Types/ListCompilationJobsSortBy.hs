{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ListCompilationJobsSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ListCompilationJobsSortBy where

import Network.AWS.Prelude

data ListCompilationJobsSortBy
  = LCJSBCreationTime
  | LCJSBName
  | LCJSBStatus
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

instance FromText ListCompilationJobsSortBy where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure LCJSBCreationTime
      "name" -> pure LCJSBName
      "status" -> pure LCJSBStatus
      e ->
        fromTextError $
          "Failure parsing ListCompilationJobsSortBy from value: '" <> e
            <> "'. Accepted values: creationtime, name, status"

instance ToText ListCompilationJobsSortBy where
  toText = \case
    LCJSBCreationTime -> "CreationTime"
    LCJSBName -> "Name"
    LCJSBStatus -> "Status"

instance Hashable ListCompilationJobsSortBy

instance NFData ListCompilationJobsSortBy

instance ToByteString ListCompilationJobsSortBy

instance ToQuery ListCompilationJobsSortBy

instance ToHeader ListCompilationJobsSortBy

instance ToJSON ListCompilationJobsSortBy where
  toJSON = toJSONText
