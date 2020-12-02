{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupCertificateMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmoothGroupCertificateMode where

import Network.AWS.Prelude

-- | Smooth Group Certificate Mode
data SmoothGroupCertificateMode
  = SGCMSelfSigned
  | SGCMVerifyAuthenticity
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

instance FromText SmoothGroupCertificateMode where
  parser =
    takeLowerText >>= \case
      "self_signed" -> pure SGCMSelfSigned
      "verify_authenticity" -> pure SGCMVerifyAuthenticity
      e ->
        fromTextError $
          "Failure parsing SmoothGroupCertificateMode from value: '" <> e
            <> "'. Accepted values: self_signed, verify_authenticity"

instance ToText SmoothGroupCertificateMode where
  toText = \case
    SGCMSelfSigned -> "SELF_SIGNED"
    SGCMVerifyAuthenticity -> "VERIFY_AUTHENTICITY"

instance Hashable SmoothGroupCertificateMode

instance NFData SmoothGroupCertificateMode

instance ToByteString SmoothGroupCertificateMode

instance ToQuery SmoothGroupCertificateMode

instance ToHeader SmoothGroupCertificateMode

instance ToJSON SmoothGroupCertificateMode where
  toJSON = toJSONText

instance FromJSON SmoothGroupCertificateMode where
  parseJSON = parseJSONText "SmoothGroupCertificateMode"
