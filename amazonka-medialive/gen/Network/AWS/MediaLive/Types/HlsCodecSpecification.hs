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
-- Module      : Network.AWS.MediaLive.Types.HlsCodecSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsCodecSpecification
  ( HlsCodecSpecification
      ( ..,
        HlsCodecSpecification_RFC_4281,
        HlsCodecSpecification_RFC_6381
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Hls Codec Specification
newtype HlsCodecSpecification = HlsCodecSpecification'
  { fromHlsCodecSpecification ::
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

pattern HlsCodecSpecification_RFC_4281 :: HlsCodecSpecification
pattern HlsCodecSpecification_RFC_4281 = HlsCodecSpecification' "RFC_4281"

pattern HlsCodecSpecification_RFC_6381 :: HlsCodecSpecification
pattern HlsCodecSpecification_RFC_6381 = HlsCodecSpecification' "RFC_6381"

{-# COMPLETE
  HlsCodecSpecification_RFC_4281,
  HlsCodecSpecification_RFC_6381,
  HlsCodecSpecification'
  #-}
