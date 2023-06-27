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
-- Module      : Amazonka.MediaConvert.Types.H264GopBReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H264GopBReference
  ( H264GopBReference
      ( ..,
        H264GopBReference_DISABLED,
        H264GopBReference_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify whether to allow B-frames to be referenced by other frame types.
-- To use reference B-frames when your GOP structure has 1 or more
-- B-frames: Leave blank or keep the default value Enabled. We recommend
-- that you choose Enabled to help improve the video quality of your output
-- relative to its bitrate. To not use reference B-frames: Choose Disabled.
newtype H264GopBReference = H264GopBReference'
  { fromH264GopBReference ::
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

pattern H264GopBReference_DISABLED :: H264GopBReference
pattern H264GopBReference_DISABLED = H264GopBReference' "DISABLED"

pattern H264GopBReference_ENABLED :: H264GopBReference
pattern H264GopBReference_ENABLED = H264GopBReference' "ENABLED"

{-# COMPLETE
  H264GopBReference_DISABLED,
  H264GopBReference_ENABLED,
  H264GopBReference'
  #-}
