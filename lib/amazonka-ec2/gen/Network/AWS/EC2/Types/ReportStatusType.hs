{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReportStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReportStatusType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ReportStatusType
  = RSTImpaired
  | RSTOK
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

instance FromText ReportStatusType where
  parser =
    takeLowerText >>= \case
      "impaired" -> pure RSTImpaired
      "ok" -> pure RSTOK
      e ->
        fromTextError $
          "Failure parsing ReportStatusType from value: '" <> e
            <> "'. Accepted values: impaired, ok"

instance ToText ReportStatusType where
  toText = \case
    RSTImpaired -> "impaired"
    RSTOK -> "ok"

instance Hashable ReportStatusType

instance NFData ReportStatusType

instance ToByteString ReportStatusType

instance ToQuery ReportStatusType

instance ToHeader ReportStatusType
