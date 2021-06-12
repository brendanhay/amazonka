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
-- Module      : Network.AWS.MediaLive.Types.RtmpCaptionData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RtmpCaptionData
  ( RtmpCaptionData
      ( ..,
        RtmpCaptionData_ALL,
        RtmpCaptionData_FIELD1_608,
        RtmpCaptionData_FIELD1_AND_FIELD2_608
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Rtmp Caption Data
newtype RtmpCaptionData = RtmpCaptionData'
  { fromRtmpCaptionData ::
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

pattern RtmpCaptionData_ALL :: RtmpCaptionData
pattern RtmpCaptionData_ALL = RtmpCaptionData' "ALL"

pattern RtmpCaptionData_FIELD1_608 :: RtmpCaptionData
pattern RtmpCaptionData_FIELD1_608 = RtmpCaptionData' "FIELD1_608"

pattern RtmpCaptionData_FIELD1_AND_FIELD2_608 :: RtmpCaptionData
pattern RtmpCaptionData_FIELD1_AND_FIELD2_608 = RtmpCaptionData' "FIELD1_AND_FIELD2_608"

{-# COMPLETE
  RtmpCaptionData_ALL,
  RtmpCaptionData_FIELD1_608,
  RtmpCaptionData_FIELD1_AND_FIELD2_608,
  RtmpCaptionData'
  #-}
