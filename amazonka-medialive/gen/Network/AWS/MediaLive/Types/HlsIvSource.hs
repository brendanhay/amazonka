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
-- Module      : Network.AWS.MediaLive.Types.HlsIvSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsIvSource
  ( HlsIvSource
      ( ..,
        HlsIvSource_EXPLICIT,
        HlsIvSource_FOLLOWS_SEGMENT_NUMBER
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Hls Iv Source
newtype HlsIvSource = HlsIvSource'
  { fromHlsIvSource ::
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

pattern HlsIvSource_EXPLICIT :: HlsIvSource
pattern HlsIvSource_EXPLICIT = HlsIvSource' "EXPLICIT"

pattern HlsIvSource_FOLLOWS_SEGMENT_NUMBER :: HlsIvSource
pattern HlsIvSource_FOLLOWS_SEGMENT_NUMBER = HlsIvSource' "FOLLOWS_SEGMENT_NUMBER"

{-# COMPLETE
  HlsIvSource_EXPLICIT,
  HlsIvSource_FOLLOWS_SEGMENT_NUMBER,
  HlsIvSource'
  #-}
