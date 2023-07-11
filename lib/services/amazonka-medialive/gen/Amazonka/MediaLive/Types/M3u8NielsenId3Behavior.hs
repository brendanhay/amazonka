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
-- Module      : Amazonka.MediaLive.Types.M3u8NielsenId3Behavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.M3u8NielsenId3Behavior
  ( M3u8NielsenId3Behavior
      ( ..,
        M3u8NielsenId3Behavior_NO_PASSTHROUGH,
        M3u8NielsenId3Behavior_PASSTHROUGH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | M3u8 Nielsen Id3 Behavior
newtype M3u8NielsenId3Behavior = M3u8NielsenId3Behavior'
  { fromM3u8NielsenId3Behavior ::
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

pattern M3u8NielsenId3Behavior_NO_PASSTHROUGH :: M3u8NielsenId3Behavior
pattern M3u8NielsenId3Behavior_NO_PASSTHROUGH = M3u8NielsenId3Behavior' "NO_PASSTHROUGH"

pattern M3u8NielsenId3Behavior_PASSTHROUGH :: M3u8NielsenId3Behavior
pattern M3u8NielsenId3Behavior_PASSTHROUGH = M3u8NielsenId3Behavior' "PASSTHROUGH"

{-# COMPLETE
  M3u8NielsenId3Behavior_NO_PASSTHROUGH,
  M3u8NielsenId3Behavior_PASSTHROUGH,
  M3u8NielsenId3Behavior'
  #-}
