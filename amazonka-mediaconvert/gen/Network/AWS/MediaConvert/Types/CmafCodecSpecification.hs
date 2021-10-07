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
-- Module      : Network.AWS.MediaConvert.Types.CmafCodecSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafCodecSpecification
  ( CmafCodecSpecification
      ( ..,
        CmafCodecSpecification_RFC_4281,
        CmafCodecSpecification_RFC_6381
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8
-- playlist generation.
newtype CmafCodecSpecification = CmafCodecSpecification'
  { fromCmafCodecSpecification ::
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

pattern CmafCodecSpecification_RFC_4281 :: CmafCodecSpecification
pattern CmafCodecSpecification_RFC_4281 = CmafCodecSpecification' "RFC_4281"

pattern CmafCodecSpecification_RFC_6381 :: CmafCodecSpecification
pattern CmafCodecSpecification_RFC_6381 = CmafCodecSpecification' "RFC_6381"

{-# COMPLETE
  CmafCodecSpecification_RFC_4281,
  CmafCodecSpecification_RFC_6381,
  CmafCodecSpecification'
  #-}
