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

import qualified Network.AWS.Prelude as Prelude

-- | Rtmp Caption Data
newtype RtmpCaptionData = RtmpCaptionData'
  { fromRtmpCaptionData ::
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
