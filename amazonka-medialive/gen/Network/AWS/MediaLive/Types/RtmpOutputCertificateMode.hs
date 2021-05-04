{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RtmpOutputCertificateMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RtmpOutputCertificateMode
  ( RtmpOutputCertificateMode
      ( ..,
        RtmpOutputCertificateMode_SELF_SIGNED,
        RtmpOutputCertificateMode_VERIFY_AUTHENTICITY
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Rtmp Output Certificate Mode
newtype RtmpOutputCertificateMode = RtmpOutputCertificateMode'
  { fromRtmpOutputCertificateMode ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern RtmpOutputCertificateMode_SELF_SIGNED :: RtmpOutputCertificateMode
pattern RtmpOutputCertificateMode_SELF_SIGNED = RtmpOutputCertificateMode' "SELF_SIGNED"

pattern RtmpOutputCertificateMode_VERIFY_AUTHENTICITY :: RtmpOutputCertificateMode
pattern RtmpOutputCertificateMode_VERIFY_AUTHENTICITY = RtmpOutputCertificateMode' "VERIFY_AUTHENTICITY"

{-# COMPLETE
  RtmpOutputCertificateMode_SELF_SIGNED,
  RtmpOutputCertificateMode_VERIFY_AUTHENTICITY,
  RtmpOutputCertificateMode'
  #-}
