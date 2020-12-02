{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CertificateSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CertificateSource where

import Network.AWS.Prelude

data CertificateSource
  = Acm
  | Cloudfront
  | IAM
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

instance FromText CertificateSource where
  parser =
    takeLowerText >>= \case
      "acm" -> pure Acm
      "cloudfront" -> pure Cloudfront
      "iam" -> pure IAM
      e ->
        fromTextError $
          "Failure parsing CertificateSource from value: '" <> e
            <> "'. Accepted values: acm, cloudfront, iam"

instance ToText CertificateSource where
  toText = \case
    Acm -> "acm"
    Cloudfront -> "cloudfront"
    IAM -> "iam"

instance Hashable CertificateSource

instance NFData CertificateSource

instance ToByteString CertificateSource

instance ToQuery CertificateSource

instance ToHeader CertificateSource

instance FromXML CertificateSource where
  parseXML = parseXMLText "CertificateSource"

instance ToXML CertificateSource where
  toXML = toXMLText
