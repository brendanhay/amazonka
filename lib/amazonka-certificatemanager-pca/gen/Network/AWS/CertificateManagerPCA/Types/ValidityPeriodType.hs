{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.ValidityPeriodType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.ValidityPeriodType where

import Network.AWS.Prelude

data ValidityPeriodType
  = Absolute
  | Days
  | EndDate
  | Months
  | Years
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

instance FromText ValidityPeriodType where
  parser =
    takeLowerText >>= \case
      "absolute" -> pure Absolute
      "days" -> pure Days
      "end_date" -> pure EndDate
      "months" -> pure Months
      "years" -> pure Years
      e ->
        fromTextError $
          "Failure parsing ValidityPeriodType from value: '" <> e
            <> "'. Accepted values: absolute, days, end_date, months, years"

instance ToText ValidityPeriodType where
  toText = \case
    Absolute -> "ABSOLUTE"
    Days -> "DAYS"
    EndDate -> "END_DATE"
    Months -> "MONTHS"
    Years -> "YEARS"

instance Hashable ValidityPeriodType

instance NFData ValidityPeriodType

instance ToByteString ValidityPeriodType

instance ToQuery ValidityPeriodType

instance ToHeader ValidityPeriodType

instance ToJSON ValidityPeriodType where
  toJSON = toJSONText
