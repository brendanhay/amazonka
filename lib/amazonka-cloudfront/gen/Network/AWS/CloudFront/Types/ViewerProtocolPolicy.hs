{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ViewerProtocolPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ViewerProtocolPolicy where

import Network.AWS.Prelude

data ViewerProtocolPolicy
  = VPPAllowAll
  | VPPHTTPSOnly
  | VPPRedirectToHTTPS
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

instance FromText ViewerProtocolPolicy where
  parser =
    takeLowerText >>= \case
      "allow-all" -> pure VPPAllowAll
      "https-only" -> pure VPPHTTPSOnly
      "redirect-to-https" -> pure VPPRedirectToHTTPS
      e ->
        fromTextError $
          "Failure parsing ViewerProtocolPolicy from value: '" <> e
            <> "'. Accepted values: allow-all, https-only, redirect-to-https"

instance ToText ViewerProtocolPolicy where
  toText = \case
    VPPAllowAll -> "allow-all"
    VPPHTTPSOnly -> "https-only"
    VPPRedirectToHTTPS -> "redirect-to-https"

instance Hashable ViewerProtocolPolicy

instance NFData ViewerProtocolPolicy

instance ToByteString ViewerProtocolPolicy

instance ToQuery ViewerProtocolPolicy

instance ToHeader ViewerProtocolPolicy

instance FromXML ViewerProtocolPolicy where
  parseXML = parseXMLText "ViewerProtocolPolicy"

instance ToXML ViewerProtocolPolicy where
  toXML = toXMLText
