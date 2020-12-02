{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FecOutputIncludeFec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FecOutputIncludeFec where

import Network.AWS.Prelude

-- | Fec Output Include Fec
data FecOutputIncludeFec
  = Column
  | ColumnAndRow
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

instance FromText FecOutputIncludeFec where
  parser =
    takeLowerText >>= \case
      "column" -> pure Column
      "column_and_row" -> pure ColumnAndRow
      e ->
        fromTextError $
          "Failure parsing FecOutputIncludeFec from value: '" <> e
            <> "'. Accepted values: column, column_and_row"

instance ToText FecOutputIncludeFec where
  toText = \case
    Column -> "COLUMN"
    ColumnAndRow -> "COLUMN_AND_ROW"

instance Hashable FecOutputIncludeFec

instance NFData FecOutputIncludeFec

instance ToByteString FecOutputIncludeFec

instance ToQuery FecOutputIncludeFec

instance ToHeader FecOutputIncludeFec

instance ToJSON FecOutputIncludeFec where
  toJSON = toJSONText

instance FromJSON FecOutputIncludeFec where
  parseJSON = parseJSONText "FecOutputIncludeFec"
