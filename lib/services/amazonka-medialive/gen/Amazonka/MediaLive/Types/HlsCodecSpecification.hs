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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Hls Codec Specification
newtype HlsCodecSpecification = HlsCodecSpecification'
  { fromHlsCodecSpecification ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
