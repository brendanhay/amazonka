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
-- Module      : Network.AWS.MediaLive.Types.M2tsNielsenId3Behavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsNielsenId3Behavior
  ( M2tsNielsenId3Behavior
      ( ..,
        M2tsNielsenId3Behavior_NO_PASSTHROUGH,
        M2tsNielsenId3Behavior_PASSTHROUGH
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | M2ts Nielsen Id3 Behavior
newtype M2tsNielsenId3Behavior = M2tsNielsenId3Behavior'
  { fromM2tsNielsenId3Behavior ::
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

pattern M2tsNielsenId3Behavior_NO_PASSTHROUGH :: M2tsNielsenId3Behavior
pattern M2tsNielsenId3Behavior_NO_PASSTHROUGH = M2tsNielsenId3Behavior' "NO_PASSTHROUGH"

pattern M2tsNielsenId3Behavior_PASSTHROUGH :: M2tsNielsenId3Behavior
pattern M2tsNielsenId3Behavior_PASSTHROUGH = M2tsNielsenId3Behavior' "PASSTHROUGH"

{-# COMPLETE
  M2tsNielsenId3Behavior_NO_PASSTHROUGH,
  M2tsNielsenId3Behavior_PASSTHROUGH,
  M2tsNielsenId3Behavior'
  #-}
