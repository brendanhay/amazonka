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
-- Module      : Amazonka.MediaLive.Types.NielsenPcmToId3TaggingState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.NielsenPcmToId3TaggingState
  ( NielsenPcmToId3TaggingState
      ( ..,
        NielsenPcmToId3TaggingState_DISABLED,
        NielsenPcmToId3TaggingState_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | State of Nielsen PCM to ID3 tagging
newtype NielsenPcmToId3TaggingState = NielsenPcmToId3TaggingState'
  { fromNielsenPcmToId3TaggingState ::
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

pattern NielsenPcmToId3TaggingState_DISABLED :: NielsenPcmToId3TaggingState
pattern NielsenPcmToId3TaggingState_DISABLED = NielsenPcmToId3TaggingState' "DISABLED"

pattern NielsenPcmToId3TaggingState_ENABLED :: NielsenPcmToId3TaggingState
pattern NielsenPcmToId3TaggingState_ENABLED = NielsenPcmToId3TaggingState' "ENABLED"

{-# COMPLETE
  NielsenPcmToId3TaggingState_DISABLED,
  NielsenPcmToId3TaggingState_ENABLED,
  NielsenPcmToId3TaggingState'
  #-}
