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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

-- | If you select ALIGN_TO_VIDEO, MediaConvert writes captions and data
-- packets with Presentation Timestamp (PTS) values greater than or equal
-- to the first video packet PTS (MediaConvert drops captions and data
-- packets with lesser PTS values). Keep the default value (AUTO) to allow
-- all PTS values.
newtype M2tsDataPtsControl = M2tsDataPtsControl'
  { fromM2tsDataPtsControl ::
      Core.Text
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

pattern M2tsDataPtsControl_ALIGN_TO_VIDEO :: M2tsDataPtsControl
pattern M2tsDataPtsControl_ALIGN_TO_VIDEO = M2tsDataPtsControl' "ALIGN_TO_VIDEO"

pattern M2tsDataPtsControl_AUTO :: M2tsDataPtsControl
pattern M2tsDataPtsControl_AUTO = M2tsDataPtsControl' "AUTO"

{-# COMPLETE
  M2tsDataPtsControl_ALIGN_TO_VIDEO,
  M2tsDataPtsControl_AUTO,
  M2tsDataPtsControl'
  #-}
