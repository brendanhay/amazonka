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
-- Module      : Amazonka.MediaLive.Types.HlsIvSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsIvSource
  ( HlsIvSource
      ( ..,
        HlsIvSource_EXPLICIT,
        HlsIvSource_FOLLOWS_SEGMENT_NUMBER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Hls Iv Source
newtype HlsIvSource = HlsIvSource'
  { fromHlsIvSource ::
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

pattern HlsIvSource_EXPLICIT :: HlsIvSource
pattern HlsIvSource_EXPLICIT = HlsIvSource' "EXPLICIT"

pattern HlsIvSource_FOLLOWS_SEGMENT_NUMBER :: HlsIvSource
pattern HlsIvSource_FOLLOWS_SEGMENT_NUMBER = HlsIvSource' "FOLLOWS_SEGMENT_NUMBER"

{-# COMPLETE
  HlsIvSource_EXPLICIT,
  HlsIvSource_FOLLOWS_SEGMENT_NUMBER,
  HlsIvSource'
  #-}
