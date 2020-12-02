{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectStorageClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectStorageClass where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ObjectStorageClass
  = OSCGlacier
  | OSCIntelligentTiering
  | OSCReducedRedundancy
  | OSCStandard
  | OSCStandardIA
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

instance FromText ObjectStorageClass where
  parser =
    takeLowerText >>= \case
      "glacier" -> pure OSCGlacier
      "intelligent_tiering" -> pure OSCIntelligentTiering
      "reduced_redundancy" -> pure OSCReducedRedundancy
      "standard" -> pure OSCStandard
      "standard_ia" -> pure OSCStandardIA
      e ->
        fromTextError $
          "Failure parsing ObjectStorageClass from value: '" <> e
            <> "'. Accepted values: glacier, intelligent_tiering, reduced_redundancy, standard, standard_ia"

instance ToText ObjectStorageClass where
  toText = \case
    OSCGlacier -> "GLACIER"
    OSCIntelligentTiering -> "INTELLIGENT_TIERING"
    OSCReducedRedundancy -> "REDUCED_REDUNDANCY"
    OSCStandard -> "STANDARD"
    OSCStandardIA -> "STANDARD_IA"

instance Hashable ObjectStorageClass

instance NFData ObjectStorageClass

instance ToByteString ObjectStorageClass

instance ToQuery ObjectStorageClass

instance ToHeader ObjectStorageClass

instance FromXML ObjectStorageClass where
  parseXML = parseXMLText "ObjectStorageClass"
