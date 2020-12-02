{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.AdditionalArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types.AdditionalArtifact where

import Network.AWS.Prelude

-- | The types of manifest that you want AWS to create for this report.
data AdditionalArtifact
  = Athena
  | Quicksight
  | Redshift
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

instance FromText AdditionalArtifact where
  parser =
    takeLowerText >>= \case
      "athena" -> pure Athena
      "quicksight" -> pure Quicksight
      "redshift" -> pure Redshift
      e ->
        fromTextError $
          "Failure parsing AdditionalArtifact from value: '" <> e
            <> "'. Accepted values: athena, quicksight, redshift"

instance ToText AdditionalArtifact where
  toText = \case
    Athena -> "ATHENA"
    Quicksight -> "QUICKSIGHT"
    Redshift -> "REDSHIFT"

instance Hashable AdditionalArtifact

instance NFData AdditionalArtifact

instance ToByteString AdditionalArtifact

instance ToQuery AdditionalArtifact

instance ToHeader AdditionalArtifact

instance ToJSON AdditionalArtifact where
  toJSON = toJSONText

instance FromJSON AdditionalArtifact where
  parseJSON = parseJSONText "AdditionalArtifact"
