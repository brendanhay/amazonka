{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.AlgorithmSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.AlgorithmSpec where

import Network.AWS.Prelude

data AlgorithmSpec
  = ASRsaesOaepSha1
  | ASRsaesOaepSha256
  | ASRsaesPKCS1V15
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

instance FromText AlgorithmSpec where
  parser =
    takeLowerText >>= \case
      "rsaes_oaep_sha_1" -> pure ASRsaesOaepSha1
      "rsaes_oaep_sha_256" -> pure ASRsaesOaepSha256
      "rsaes_pkcs1_v1_5" -> pure ASRsaesPKCS1V15
      e ->
        fromTextError $
          "Failure parsing AlgorithmSpec from value: '" <> e
            <> "'. Accepted values: rsaes_oaep_sha_1, rsaes_oaep_sha_256, rsaes_pkcs1_v1_5"

instance ToText AlgorithmSpec where
  toText = \case
    ASRsaesOaepSha1 -> "RSAES_OAEP_SHA_1"
    ASRsaesOaepSha256 -> "RSAES_OAEP_SHA_256"
    ASRsaesPKCS1V15 -> "RSAES_PKCS1_V1_5"

instance Hashable AlgorithmSpec

instance NFData AlgorithmSpec

instance ToByteString AlgorithmSpec

instance ToQuery AlgorithmSpec

instance ToHeader AlgorithmSpec

instance ToJSON AlgorithmSpec where
  toJSON = toJSONText
