{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSortKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSortKey where

import Network.AWS.Prelude

data NotebookInstanceLifecycleConfigSortKey
  = NILCSKCreationTime
  | NILCSKLastModifiedTime
  | NILCSKName
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

instance FromText NotebookInstanceLifecycleConfigSortKey where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure NILCSKCreationTime
      "lastmodifiedtime" -> pure NILCSKLastModifiedTime
      "name" -> pure NILCSKName
      e ->
        fromTextError $
          "Failure parsing NotebookInstanceLifecycleConfigSortKey from value: '" <> e
            <> "'. Accepted values: creationtime, lastmodifiedtime, name"

instance ToText NotebookInstanceLifecycleConfigSortKey where
  toText = \case
    NILCSKCreationTime -> "CreationTime"
    NILCSKLastModifiedTime -> "LastModifiedTime"
    NILCSKName -> "Name"

instance Hashable NotebookInstanceLifecycleConfigSortKey

instance NFData NotebookInstanceLifecycleConfigSortKey

instance ToByteString NotebookInstanceLifecycleConfigSortKey

instance ToQuery NotebookInstanceLifecycleConfigSortKey

instance ToHeader NotebookInstanceLifecycleConfigSortKey

instance ToJSON NotebookInstanceLifecycleConfigSortKey where
  toJSON = toJSONText
