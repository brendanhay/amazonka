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
-- Module      : Network.AWS.CodeBuild.Types.CacheMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.CacheMode
  ( CacheMode
      ( ..,
        CacheMode_LOCAL_CUSTOM_CACHE,
        CacheMode_LOCAL_DOCKER_LAYER_CACHE,
        CacheMode_LOCAL_SOURCE_CACHE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype CacheMode = CacheMode'
  { fromCacheMode ::
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

pattern CacheMode_LOCAL_CUSTOM_CACHE :: CacheMode
pattern CacheMode_LOCAL_CUSTOM_CACHE = CacheMode' "LOCAL_CUSTOM_CACHE"

pattern CacheMode_LOCAL_DOCKER_LAYER_CACHE :: CacheMode
pattern CacheMode_LOCAL_DOCKER_LAYER_CACHE = CacheMode' "LOCAL_DOCKER_LAYER_CACHE"

pattern CacheMode_LOCAL_SOURCE_CACHE :: CacheMode
pattern CacheMode_LOCAL_SOURCE_CACHE = CacheMode' "LOCAL_SOURCE_CACHE"

{-# COMPLETE
  CacheMode_LOCAL_CUSTOM_CACHE,
  CacheMode_LOCAL_DOCKER_LAYER_CACHE,
  CacheMode_LOCAL_SOURCE_CACHE,
  CacheMode'
  #-}
