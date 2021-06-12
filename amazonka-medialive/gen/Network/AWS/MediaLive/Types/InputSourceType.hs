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
-- Module      : Network.AWS.MediaLive.Types.InputSourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSourceType
  ( InputSourceType
      ( ..,
        InputSourceType_DYNAMIC,
        InputSourceType_STATIC
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | There are two types of input sources, static and dynamic. If an input
-- source is dynamic you can change the source url of the input dynamically
-- using an input switch action. However, the only input type to support a
-- dynamic url at this time is MP4_FILE. By default all input sources are
-- static.
newtype InputSourceType = InputSourceType'
  { fromInputSourceType ::
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

pattern InputSourceType_DYNAMIC :: InputSourceType
pattern InputSourceType_DYNAMIC = InputSourceType' "DYNAMIC"

pattern InputSourceType_STATIC :: InputSourceType
pattern InputSourceType_STATIC = InputSourceType' "STATIC"

{-# COMPLETE
  InputSourceType_DYNAMIC,
  InputSourceType_STATIC,
  InputSourceType'
  #-}
