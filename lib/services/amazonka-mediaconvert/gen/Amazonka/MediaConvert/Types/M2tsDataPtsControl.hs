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
-- Module      : Amazonka.MediaConvert.Types.M2tsDataPtsControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.M2tsDataPtsControl
  ( M2tsDataPtsControl
      ( ..,
        M2tsDataPtsControl_ALIGN_TO_VIDEO,
        M2tsDataPtsControl_AUTO
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
newtype M2tsDataPtsControl = M2tsDataPtsControl'
  { fromM2tsDataPtsControl ::
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

pattern M2tsDataPtsControl_ALIGN_TO_VIDEO :: M2tsDataPtsControl
pattern M2tsDataPtsControl_ALIGN_TO_VIDEO = M2tsDataPtsControl' "ALIGN_TO_VIDEO"

pattern M2tsDataPtsControl_AUTO :: M2tsDataPtsControl
pattern M2tsDataPtsControl_AUTO = M2tsDataPtsControl' "AUTO"

{-# COMPLETE
  M2tsDataPtsControl_ALIGN_TO_VIDEO,
  M2tsDataPtsControl_AUTO,
  M2tsDataPtsControl'
  #-}
