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
-- Module      : Amazonka.MediaConvert.Types.M3u8DataPtsControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.M3u8DataPtsControl
  ( M3u8DataPtsControl
      ( ..,
        M3u8DataPtsControl_ALIGN_TO_VIDEO,
        M3u8DataPtsControl_AUTO
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | If you select ALIGN_TO_VIDEO, MediaConvert writes captions and data
-- packets with Presentation Timestamp (PTS) values greater than or equal
-- to the first video packet PTS (MediaConvert drops captions and data
-- packets with lesser PTS values). Keep the default value (AUTO) to allow
-- all PTS values.
newtype M3u8DataPtsControl = M3u8DataPtsControl'
  { fromM3u8DataPtsControl ::
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

pattern M3u8DataPtsControl_ALIGN_TO_VIDEO :: M3u8DataPtsControl
pattern M3u8DataPtsControl_ALIGN_TO_VIDEO = M3u8DataPtsControl' "ALIGN_TO_VIDEO"

pattern M3u8DataPtsControl_AUTO :: M3u8DataPtsControl
pattern M3u8DataPtsControl_AUTO = M3u8DataPtsControl' "AUTO"

{-# COMPLETE
  M3u8DataPtsControl_ALIGN_TO_VIDEO,
  M3u8DataPtsControl_AUTO,
  M3u8DataPtsControl'
  #-}
