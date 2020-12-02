{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectVersionStorageClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectVersionStorageClass where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ObjectVersionStorageClass = OVSCStandard
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

instance FromText ObjectVersionStorageClass where
  parser =
    takeLowerText >>= \case
      "standard" -> pure OVSCStandard
      e ->
        fromTextError $
          "Failure parsing ObjectVersionStorageClass from value: '" <> e
            <> "'. Accepted values: standard"

instance ToText ObjectVersionStorageClass where
  toText = \case
    OVSCStandard -> "STANDARD"

instance Hashable ObjectVersionStorageClass

instance NFData ObjectVersionStorageClass

instance ToByteString ObjectVersionStorageClass

instance ToQuery ObjectVersionStorageClass

instance ToHeader ObjectVersionStorageClass

instance FromXML ObjectVersionStorageClass where
  parseXML = parseXMLText "ObjectVersionStorageClass"
