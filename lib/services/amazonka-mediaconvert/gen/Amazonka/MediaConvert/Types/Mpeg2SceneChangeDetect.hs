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
-- Module      : Amazonka.MediaConvert.Types.Mpeg2SceneChangeDetect
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Mpeg2SceneChangeDetect
  ( Mpeg2SceneChangeDetect
      ( ..,
        Mpeg2SceneChangeDetect_DISABLED,
        Mpeg2SceneChangeDetect_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Enable this setting to insert I-frames at scene changes that the service
-- automatically detects. This improves video quality and is enabled by
-- default.
newtype Mpeg2SceneChangeDetect = Mpeg2SceneChangeDetect'
  { fromMpeg2SceneChangeDetect ::
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

pattern Mpeg2SceneChangeDetect_DISABLED :: Mpeg2SceneChangeDetect
pattern Mpeg2SceneChangeDetect_DISABLED = Mpeg2SceneChangeDetect' "DISABLED"

pattern Mpeg2SceneChangeDetect_ENABLED :: Mpeg2SceneChangeDetect
pattern Mpeg2SceneChangeDetect_ENABLED = Mpeg2SceneChangeDetect' "ENABLED"

{-# COMPLETE
  Mpeg2SceneChangeDetect_DISABLED,
  Mpeg2SceneChangeDetect_ENABLED,
  Mpeg2SceneChangeDetect'
  #-}
