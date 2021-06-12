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
-- Module      : Network.AWS.MediaLive.Types.Mpeg2ScanType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2ScanType
  ( Mpeg2ScanType
      ( ..,
        Mpeg2ScanType_INTERLACED,
        Mpeg2ScanType_PROGRESSIVE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Mpeg2 Scan Type
newtype Mpeg2ScanType = Mpeg2ScanType'
  { fromMpeg2ScanType ::
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

pattern Mpeg2ScanType_INTERLACED :: Mpeg2ScanType
pattern Mpeg2ScanType_INTERLACED = Mpeg2ScanType' "INTERLACED"

pattern Mpeg2ScanType_PROGRESSIVE :: Mpeg2ScanType
pattern Mpeg2ScanType_PROGRESSIVE = Mpeg2ScanType' "PROGRESSIVE"

{-# COMPLETE
  Mpeg2ScanType_INTERLACED,
  Mpeg2ScanType_PROGRESSIVE,
  Mpeg2ScanType'
  #-}
