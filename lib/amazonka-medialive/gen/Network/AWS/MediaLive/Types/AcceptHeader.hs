{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AcceptHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AcceptHeader where

import Network.AWS.Prelude

-- | The HTTP Accept header. Indicates the requested type fothe thumbnail.
data AcceptHeader = AHImageJpeg
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

instance FromText AcceptHeader where
  parser =
    takeLowerText >>= \case
      "image/jpeg" -> pure AHImageJpeg
      e ->
        fromTextError $
          "Failure parsing AcceptHeader from value: '" <> e
            <> "'. Accepted values: image/jpeg"

instance ToText AcceptHeader where
  toText = \case
    AHImageJpeg -> "image/jpeg"

instance Hashable AcceptHeader

instance NFData AcceptHeader

instance ToByteString AcceptHeader

instance ToQuery AcceptHeader

instance ToHeader AcceptHeader

instance ToJSON AcceptHeader where
  toJSON = toJSONText
