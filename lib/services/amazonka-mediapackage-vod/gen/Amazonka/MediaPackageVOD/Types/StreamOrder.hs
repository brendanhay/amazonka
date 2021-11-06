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
-- Module      : Amazonka.MediaPackageVOD.Types.StreamOrder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.StreamOrder
  ( StreamOrder
      ( ..,
        StreamOrder_ORIGINAL,
        StreamOrder_VIDEO_BITRATE_ASCENDING,
        StreamOrder_VIDEO_BITRATE_DESCENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype StreamOrder = StreamOrder'
  { fromStreamOrder ::
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

pattern StreamOrder_ORIGINAL :: StreamOrder
pattern StreamOrder_ORIGINAL = StreamOrder' "ORIGINAL"

pattern StreamOrder_VIDEO_BITRATE_ASCENDING :: StreamOrder
pattern StreamOrder_VIDEO_BITRATE_ASCENDING = StreamOrder' "VIDEO_BITRATE_ASCENDING"

pattern StreamOrder_VIDEO_BITRATE_DESCENDING :: StreamOrder
pattern StreamOrder_VIDEO_BITRATE_DESCENDING = StreamOrder' "VIDEO_BITRATE_DESCENDING"

{-# COMPLETE
  StreamOrder_ORIGINAL,
  StreamOrder_VIDEO_BITRATE_ASCENDING,
  StreamOrder_VIDEO_BITRATE_DESCENDING,
  StreamOrder'
  #-}
