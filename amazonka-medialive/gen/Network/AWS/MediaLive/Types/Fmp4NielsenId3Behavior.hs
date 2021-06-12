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
-- Module      : Network.AWS.MediaLive.Types.Fmp4NielsenId3Behavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Fmp4NielsenId3Behavior
  ( Fmp4NielsenId3Behavior
      ( ..,
        Fmp4NielsenId3Behavior_NO_PASSTHROUGH,
        Fmp4NielsenId3Behavior_PASSTHROUGH
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Fmp4 Nielsen Id3 Behavior
newtype Fmp4NielsenId3Behavior = Fmp4NielsenId3Behavior'
  { fromFmp4NielsenId3Behavior ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern Fmp4NielsenId3Behavior_NO_PASSTHROUGH :: Fmp4NielsenId3Behavior
pattern Fmp4NielsenId3Behavior_NO_PASSTHROUGH = Fmp4NielsenId3Behavior' "NO_PASSTHROUGH"

pattern Fmp4NielsenId3Behavior_PASSTHROUGH :: Fmp4NielsenId3Behavior
pattern Fmp4NielsenId3Behavior_PASSTHROUGH = Fmp4NielsenId3Behavior' "PASSTHROUGH"

{-# COMPLETE
  Fmp4NielsenId3Behavior_NO_PASSTHROUGH,
  Fmp4NielsenId3Behavior_PASSTHROUGH,
  Fmp4NielsenId3Behavior'
  #-}
