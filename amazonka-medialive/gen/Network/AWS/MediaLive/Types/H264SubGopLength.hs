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
-- Module      : Network.AWS.MediaLive.Types.H264SubGopLength
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264SubGopLength
  ( H264SubGopLength
      ( ..,
        H264SubGopLength_DYNAMIC,
        H264SubGopLength_FIXED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | H264 Sub Gop Length
newtype H264SubGopLength = H264SubGopLength'
  { fromH264SubGopLength ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern H264SubGopLength_DYNAMIC :: H264SubGopLength
pattern H264SubGopLength_DYNAMIC = H264SubGopLength' "DYNAMIC"

pattern H264SubGopLength_FIXED :: H264SubGopLength
pattern H264SubGopLength_FIXED = H264SubGopLength' "FIXED"

{-# COMPLETE
  H264SubGopLength_DYNAMIC,
  H264SubGopLength_FIXED,
  H264SubGopLength'
  #-}
