{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ListWorkforcesSortByOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ListWorkforcesSortByOptions where

import Network.AWS.Prelude

data ListWorkforcesSortByOptions
  = LWSBOCreateDate
  | LWSBOName
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

instance FromText ListWorkforcesSortByOptions where
  parser =
    takeLowerText >>= \case
      "createdate" -> pure LWSBOCreateDate
      "name" -> pure LWSBOName
      e ->
        fromTextError $
          "Failure parsing ListWorkforcesSortByOptions from value: '" <> e
            <> "'. Accepted values: createdate, name"

instance ToText ListWorkforcesSortByOptions where
  toText = \case
    LWSBOCreateDate -> "CreateDate"
    LWSBOName -> "Name"

instance Hashable ListWorkforcesSortByOptions

instance NFData ListWorkforcesSortByOptions

instance ToByteString ListWorkforcesSortByOptions

instance ToQuery ListWorkforcesSortByOptions

instance ToHeader ListWorkforcesSortByOptions

instance ToJSON ListWorkforcesSortByOptions where
  toJSON = toJSONText
