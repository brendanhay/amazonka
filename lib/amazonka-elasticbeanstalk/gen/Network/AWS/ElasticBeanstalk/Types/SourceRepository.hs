{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.SourceRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SourceRepository where

import Network.AWS.Prelude

data SourceRepository
  = CodeCommit
  | S3
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

instance FromText SourceRepository where
  parser =
    takeLowerText >>= \case
      "codecommit" -> pure CodeCommit
      "s3" -> pure S3
      e ->
        fromTextError $
          "Failure parsing SourceRepository from value: '" <> e
            <> "'. Accepted values: codecommit, s3"

instance ToText SourceRepository where
  toText = \case
    CodeCommit -> "CodeCommit"
    S3 -> "S3"

instance Hashable SourceRepository

instance NFData SourceRepository

instance ToByteString SourceRepository

instance ToQuery SourceRepository

instance ToHeader SourceRepository

instance FromXML SourceRepository where
  parseXML = parseXMLText "SourceRepository"
