{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticGpuStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticGpuStatus where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ElasticGpuStatus
  = EGSImpaired
  | EGSOK
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

instance FromText ElasticGpuStatus where
  parser =
    takeLowerText >>= \case
      "impaired" -> pure EGSImpaired
      "ok" -> pure EGSOK
      e ->
        fromTextError $
          "Failure parsing ElasticGpuStatus from value: '" <> e
            <> "'. Accepted values: impaired, ok"

instance ToText ElasticGpuStatus where
  toText = \case
    EGSImpaired -> "IMPAIRED"
    EGSOK -> "OK"

instance Hashable ElasticGpuStatus

instance NFData ElasticGpuStatus

instance ToByteString ElasticGpuStatus

instance ToQuery ElasticGpuStatus

instance ToHeader ElasticGpuStatus

instance FromXML ElasticGpuStatus where
  parseXML = parseXMLText "ElasticGpuStatus"
