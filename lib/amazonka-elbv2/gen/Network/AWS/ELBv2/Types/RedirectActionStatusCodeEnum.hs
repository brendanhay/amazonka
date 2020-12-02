{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.RedirectActionStatusCodeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.RedirectActionStatusCodeEnum where

import Network.AWS.Prelude

data RedirectActionStatusCodeEnum
  = HTTP301
  | HTTP302
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

instance FromText RedirectActionStatusCodeEnum where
  parser =
    takeLowerText >>= \case
      "http_301" -> pure HTTP301
      "http_302" -> pure HTTP302
      e ->
        fromTextError $
          "Failure parsing RedirectActionStatusCodeEnum from value: '" <> e
            <> "'. Accepted values: http_301, http_302"

instance ToText RedirectActionStatusCodeEnum where
  toText = \case
    HTTP301 -> "HTTP_301"
    HTTP302 -> "HTTP_302"

instance Hashable RedirectActionStatusCodeEnum

instance NFData RedirectActionStatusCodeEnum

instance ToByteString RedirectActionStatusCodeEnum

instance ToQuery RedirectActionStatusCodeEnum

instance ToHeader RedirectActionStatusCodeEnum

instance FromXML RedirectActionStatusCodeEnum where
  parseXML = parseXMLText "RedirectActionStatusCodeEnum"
