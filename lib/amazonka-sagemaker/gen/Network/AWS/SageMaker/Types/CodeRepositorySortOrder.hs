{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CodeRepositorySortOrder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CodeRepositorySortOrder where

import Network.AWS.Prelude

data CodeRepositorySortOrder
  = CRSOAscending
  | CRSODescending
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

instance FromText CodeRepositorySortOrder where
  parser =
    takeLowerText >>= \case
      "ascending" -> pure CRSOAscending
      "descending" -> pure CRSODescending
      e ->
        fromTextError $
          "Failure parsing CodeRepositorySortOrder from value: '" <> e
            <> "'. Accepted values: ascending, descending"

instance ToText CodeRepositorySortOrder where
  toText = \case
    CRSOAscending -> "Ascending"
    CRSODescending -> "Descending"

instance Hashable CodeRepositorySortOrder

instance NFData CodeRepositorySortOrder

instance ToByteString CodeRepositorySortOrder

instance ToQuery CodeRepositorySortOrder

instance ToHeader CodeRepositorySortOrder

instance ToJSON CodeRepositorySortOrder where
  toJSON = toJSONText
