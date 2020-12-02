{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceSortKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceSortKey where

import Network.AWS.Prelude

data NotebookInstanceSortKey
  = NISKCreationTime
  | NISKName
  | NISKStatus
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

instance FromText NotebookInstanceSortKey where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure NISKCreationTime
      "name" -> pure NISKName
      "status" -> pure NISKStatus
      e ->
        fromTextError $
          "Failure parsing NotebookInstanceSortKey from value: '" <> e
            <> "'. Accepted values: creationtime, name, status"

instance ToText NotebookInstanceSortKey where
  toText = \case
    NISKCreationTime -> "CreationTime"
    NISKName -> "Name"
    NISKStatus -> "Status"

instance Hashable NotebookInstanceSortKey

instance NFData NotebookInstanceSortKey

instance ToByteString NotebookInstanceSortKey

instance ToQuery NotebookInstanceSortKey

instance ToHeader NotebookInstanceSortKey

instance ToJSON NotebookInstanceSortKey where
  toJSON = toJSONText
