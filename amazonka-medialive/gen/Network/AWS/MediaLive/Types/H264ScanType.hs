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
-- Module      : Network.AWS.MediaLive.Types.H264ScanType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264ScanType
  ( H264ScanType
      ( ..,
        H264ScanType_INTERLACED,
        H264ScanType_PROGRESSIVE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | H264 Scan Type
newtype H264ScanType = H264ScanType'
  { fromH264ScanType ::
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

pattern H264ScanType_INTERLACED :: H264ScanType
pattern H264ScanType_INTERLACED = H264ScanType' "INTERLACED"

pattern H264ScanType_PROGRESSIVE :: H264ScanType
pattern H264ScanType_PROGRESSIVE = H264ScanType' "PROGRESSIVE"

{-# COMPLETE
  H264ScanType_INTERLACED,
  H264ScanType_PROGRESSIVE,
  H264ScanType'
  #-}
