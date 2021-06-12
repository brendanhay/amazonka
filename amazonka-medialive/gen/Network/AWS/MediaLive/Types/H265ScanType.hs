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
-- Module      : Network.AWS.MediaLive.Types.H265ScanType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265ScanType
  ( H265ScanType
      ( ..,
        H265ScanType_INTERLACED,
        H265ScanType_PROGRESSIVE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | H265 Scan Type
newtype H265ScanType = H265ScanType'
  { fromH265ScanType ::
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

pattern H265ScanType_INTERLACED :: H265ScanType
pattern H265ScanType_INTERLACED = H265ScanType' "INTERLACED"

pattern H265ScanType_PROGRESSIVE :: H265ScanType
pattern H265ScanType_PROGRESSIVE = H265ScanType' "PROGRESSIVE"

{-# COMPLETE
  H265ScanType_INTERLACED,
  H265ScanType_PROGRESSIVE,
  H265ScanType'
  #-}
