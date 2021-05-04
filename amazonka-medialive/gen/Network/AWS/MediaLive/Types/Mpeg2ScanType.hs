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

import qualified Network.AWS.Prelude as Prelude

-- | Mpeg2 Scan Type
newtype Mpeg2ScanType = Mpeg2ScanType'
  { fromMpeg2ScanType ::
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

pattern Mpeg2ScanType_INTERLACED :: Mpeg2ScanType
pattern Mpeg2ScanType_INTERLACED = Mpeg2ScanType' "INTERLACED"

pattern Mpeg2ScanType_PROGRESSIVE :: Mpeg2ScanType
pattern Mpeg2ScanType_PROGRESSIVE = Mpeg2ScanType' "PROGRESSIVE"

{-# COMPLETE
  Mpeg2ScanType_INTERLACED,
  Mpeg2ScanType_PROGRESSIVE,
  Mpeg2ScanType'
  #-}
