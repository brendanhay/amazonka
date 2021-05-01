{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | H264 Scan Type
newtype H264ScanType = H264ScanType'
  { fromH264ScanType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
