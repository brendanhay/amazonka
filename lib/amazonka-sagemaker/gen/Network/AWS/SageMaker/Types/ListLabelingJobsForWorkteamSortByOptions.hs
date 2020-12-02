{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ListLabelingJobsForWorkteamSortByOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ListLabelingJobsForWorkteamSortByOptions where

import Network.AWS.Prelude

data ListLabelingJobsForWorkteamSortByOptions = LLJFWSBOCreationTime
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

instance FromText ListLabelingJobsForWorkteamSortByOptions where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure LLJFWSBOCreationTime
      e ->
        fromTextError $
          "Failure parsing ListLabelingJobsForWorkteamSortByOptions from value: '" <> e
            <> "'. Accepted values: creationtime"

instance ToText ListLabelingJobsForWorkteamSortByOptions where
  toText = \case
    LLJFWSBOCreationTime -> "CreationTime"

instance Hashable ListLabelingJobsForWorkteamSortByOptions

instance NFData ListLabelingJobsForWorkteamSortByOptions

instance ToByteString ListLabelingJobsForWorkteamSortByOptions

instance ToQuery ListLabelingJobsForWorkteamSortByOptions

instance ToHeader ListLabelingJobsForWorkteamSortByOptions

instance ToJSON ListLabelingJobsForWorkteamSortByOptions where
  toJSON = toJSONText
