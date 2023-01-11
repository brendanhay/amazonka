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
-- Module      : Amazonka.DeviceFarm.Types.InteractionMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.InteractionMode
  ( InteractionMode
      ( ..,
        InteractionMode_INTERACTIVE,
        InteractionMode_NO_VIDEO,
        InteractionMode_VIDEO_ONLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InteractionMode = InteractionMode'
  { fromInteractionMode ::
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

pattern InteractionMode_INTERACTIVE :: InteractionMode
pattern InteractionMode_INTERACTIVE = InteractionMode' "INTERACTIVE"

pattern InteractionMode_NO_VIDEO :: InteractionMode
pattern InteractionMode_NO_VIDEO = InteractionMode' "NO_VIDEO"

pattern InteractionMode_VIDEO_ONLY :: InteractionMode
pattern InteractionMode_VIDEO_ONLY = InteractionMode' "VIDEO_ONLY"

{-# COMPLETE
  InteractionMode_INTERACTIVE,
  InteractionMode_NO_VIDEO,
  InteractionMode_VIDEO_ONLY,
  InteractionMode'
  #-}
