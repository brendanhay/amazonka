{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SummaryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SummaryStatus where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data SummaryStatus
  = SSImpaired
  | SSInitializing
  | SSInsufficientData
  | SSNotApplicable
  | SSOK
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

instance FromText SummaryStatus where
  parser =
    takeLowerText >>= \case
      "impaired" -> pure SSImpaired
      "initializing" -> pure SSInitializing
      "insufficient-data" -> pure SSInsufficientData
      "not-applicable" -> pure SSNotApplicable
      "ok" -> pure SSOK
      e ->
        fromTextError $
          "Failure parsing SummaryStatus from value: '" <> e
            <> "'. Accepted values: impaired, initializing, insufficient-data, not-applicable, ok"

instance ToText SummaryStatus where
  toText = \case
    SSImpaired -> "impaired"
    SSInitializing -> "initializing"
    SSInsufficientData -> "insufficient-data"
    SSNotApplicable -> "not-applicable"
    SSOK -> "ok"

instance Hashable SummaryStatus

instance NFData SummaryStatus

instance ToByteString SummaryStatus

instance ToQuery SummaryStatus

instance ToHeader SummaryStatus

instance FromXML SummaryStatus where
  parseXML = parseXMLText "SummaryStatus"
