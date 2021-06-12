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
-- Module      : Network.AWS.MediaConvert.Types.AntiAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AntiAlias
  ( AntiAlias
      ( ..,
        AntiAlias_DISABLED,
        AntiAlias_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The anti-alias filter is automatically applied to all outputs. The
-- service no longer accepts the value DISABLED for AntiAlias. If you
-- specify that in your job, the service will ignore the setting.
newtype AntiAlias = AntiAlias'
  { fromAntiAlias ::
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

pattern AntiAlias_DISABLED :: AntiAlias
pattern AntiAlias_DISABLED = AntiAlias' "DISABLED"

pattern AntiAlias_ENABLED :: AntiAlias
pattern AntiAlias_ENABLED = AntiAlias' "ENABLED"

{-# COMPLETE
  AntiAlias_DISABLED,
  AntiAlias_ENABLED,
  AntiAlias'
  #-}
