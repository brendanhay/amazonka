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
-- Module      : Network.AWS.MediaConvert.Types.CmafClientCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafClientCache
  ( CmafClientCache
      ( ..,
        CmafClientCache_DISABLED,
        CmafClientCache_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Disable this setting only when your workflow requires the
-- #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled
-- (ENABLED) and control caching in your video distribution set up. For
-- example, use the Cache-Control http header.
newtype CmafClientCache = CmafClientCache'
  { fromCmafClientCache ::
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

pattern CmafClientCache_DISABLED :: CmafClientCache
pattern CmafClientCache_DISABLED = CmafClientCache' "DISABLED"

pattern CmafClientCache_ENABLED :: CmafClientCache
pattern CmafClientCache_ENABLED = CmafClientCache' "ENABLED"

{-# COMPLETE
  CmafClientCache_DISABLED,
  CmafClientCache_ENABLED,
  CmafClientCache'
  #-}
