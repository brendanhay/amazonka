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
-- Module      : Amazonka.MediaLive.Types.RtmpCaptionData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.RtmpCaptionData
  ( RtmpCaptionData
      ( ..,
        RtmpCaptionData_ALL,
        RtmpCaptionData_FIELD1_608,
        RtmpCaptionData_FIELD1_AND_FIELD2_608
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Rtmp Caption Data
newtype RtmpCaptionData = RtmpCaptionData'
  { fromRtmpCaptionData ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
