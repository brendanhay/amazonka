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
-- Module      : Network.AWS.MediaLive.Types.Mpeg2SubGopLength
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2SubGopLength
  ( Mpeg2SubGopLength
      ( ..,
        Mpeg2SubGopLength_DYNAMIC,
        Mpeg2SubGopLength_FIXED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Mpeg2 Sub Gop Length
newtype Mpeg2SubGopLength = Mpeg2SubGopLength'
  { fromMpeg2SubGopLength ::
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

pattern Mpeg2SubGopLength_DYNAMIC :: Mpeg2SubGopLength
pattern Mpeg2SubGopLength_DYNAMIC = Mpeg2SubGopLength' "DYNAMIC"

pattern Mpeg2SubGopLength_FIXED :: Mpeg2SubGopLength
pattern Mpeg2SubGopLength_FIXED = Mpeg2SubGopLength' "FIXED"

{-# COMPLETE
  Mpeg2SubGopLength_DYNAMIC,
  Mpeg2SubGopLength_FIXED,
  Mpeg2SubGopLength'
  #-}
