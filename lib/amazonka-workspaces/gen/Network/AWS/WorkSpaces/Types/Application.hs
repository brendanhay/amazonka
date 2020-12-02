{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Application
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.Application where

import Network.AWS.Prelude

data Application
  = MicrosoftOffice2016
  | MicrosoftOffice2019
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

instance FromText Application where
  parser =
    takeLowerText >>= \case
      "microsoft_office_2016" -> pure MicrosoftOffice2016
      "microsoft_office_2019" -> pure MicrosoftOffice2019
      e ->
        fromTextError $
          "Failure parsing Application from value: '" <> e
            <> "'. Accepted values: microsoft_office_2016, microsoft_office_2019"

instance ToText Application where
  toText = \case
    MicrosoftOffice2016 -> "Microsoft_Office_2016"
    MicrosoftOffice2019 -> "Microsoft_Office_2019"

instance Hashable Application

instance NFData Application

instance ToByteString Application

instance ToQuery Application

instance ToHeader Application

instance ToJSON Application where
  toJSON = toJSONText
