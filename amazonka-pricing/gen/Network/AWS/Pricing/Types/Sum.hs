{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pricing.Types.Sum where

import Network.AWS.Prelude

data FilterType =
  TermMatch
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FilterType where
    parser = takeLowerText >>= \case
        "term_match" -> pure TermMatch
        e -> fromTextError $ "Failure parsing FilterType from value: '" <> e
           <> "'. Accepted values: term_match"

instance ToText FilterType where
    toText = \case
        TermMatch -> "TERM_MATCH"

instance Hashable     FilterType
instance NFData       FilterType
instance ToByteString FilterType
instance ToQuery      FilterType
instance ToHeader     FilterType

instance ToJSON FilterType where
    toJSON = toJSONText
