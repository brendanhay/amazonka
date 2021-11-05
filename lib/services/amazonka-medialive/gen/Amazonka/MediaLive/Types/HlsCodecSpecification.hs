{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaLive.Types.HlsCodecSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsCodecSpecification
  ( HlsCodecSpecification
      ( ..,
        HlsCodecSpecification_RFC_4281,
        HlsCodecSpecification_RFC_6381
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Hls Codec Specification
newtype HlsCodecSpecification = HlsCodecSpecification'
  { fromHlsCodecSpecification ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
