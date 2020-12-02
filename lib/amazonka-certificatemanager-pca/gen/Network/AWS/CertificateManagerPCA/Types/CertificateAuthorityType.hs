{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityType where

import Network.AWS.Prelude

data CertificateAuthorityType
  = Root
  | Subordinate
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

instance FromText CertificateAuthorityType where
  parser =
    takeLowerText >>= \case
      "root" -> pure Root
      "subordinate" -> pure Subordinate
      e ->
        fromTextError $
          "Failure parsing CertificateAuthorityType from value: '" <> e
            <> "'. Accepted values: root, subordinate"

instance ToText CertificateAuthorityType where
  toText = \case
    Root -> "ROOT"
    Subordinate -> "SUBORDINATE"

instance Hashable CertificateAuthorityType

instance NFData CertificateAuthorityType

instance ToByteString CertificateAuthorityType

instance ToQuery CertificateAuthorityType

instance ToHeader CertificateAuthorityType

instance ToJSON CertificateAuthorityType where
  toJSON = toJSONText

instance FromJSON CertificateAuthorityType where
  parseJSON = parseJSONText "CertificateAuthorityType"
