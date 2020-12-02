{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RtmpOutputCertificateMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RtmpOutputCertificateMode where

import Network.AWS.Prelude

-- | Rtmp Output Certificate Mode
data RtmpOutputCertificateMode
  = SelfSigned
  | VerifyAuthenticity
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

instance FromText RtmpOutputCertificateMode where
  parser =
    takeLowerText >>= \case
      "self_signed" -> pure SelfSigned
      "verify_authenticity" -> pure VerifyAuthenticity
      e ->
        fromTextError $
          "Failure parsing RtmpOutputCertificateMode from value: '" <> e
            <> "'. Accepted values: self_signed, verify_authenticity"

instance ToText RtmpOutputCertificateMode where
  toText = \case
    SelfSigned -> "SELF_SIGNED"
    VerifyAuthenticity -> "VERIFY_AUTHENTICITY"

instance Hashable RtmpOutputCertificateMode

instance NFData RtmpOutputCertificateMode

instance ToByteString RtmpOutputCertificateMode

instance ToQuery RtmpOutputCertificateMode

instance ToHeader RtmpOutputCertificateMode

instance ToJSON RtmpOutputCertificateMode where
  toJSON = toJSONText

instance FromJSON RtmpOutputCertificateMode where
  parseJSON = parseJSONText "RtmpOutputCertificateMode"
