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
-- Module      : Network.AWS.MediaLive.Types.M2tsScte35Control
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsScte35Control
  ( M2tsScte35Control
      ( ..,
        M2tsScte35Control_NONE,
        M2tsScte35Control_PASSTHROUGH
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | M2ts Scte35 Control
newtype M2tsScte35Control = M2tsScte35Control'
  { fromM2tsScte35Control ::
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

pattern M2tsScte35Control_NONE :: M2tsScte35Control
pattern M2tsScte35Control_NONE = M2tsScte35Control' "NONE"

pattern M2tsScte35Control_PASSTHROUGH :: M2tsScte35Control
pattern M2tsScte35Control_PASSTHROUGH = M2tsScte35Control' "PASSTHROUGH"

{-# COMPLETE
  M2tsScte35Control_NONE,
  M2tsScte35Control_PASSTHROUGH,
  M2tsScte35Control'
  #-}
