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
-- Module      : Network.AWS.MediaPackage.Types.StreamOrder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.StreamOrder
  ( StreamOrder
      ( ..,
        StreamOrder_ORIGINAL,
        StreamOrder_VIDEO_BITRATE_ASCENDING,
        StreamOrder_VIDEO_BITRATE_DESCENDING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype StreamOrder = StreamOrder'
  { fromStreamOrder ::
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
