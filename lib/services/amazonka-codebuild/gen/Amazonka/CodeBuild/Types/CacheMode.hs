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
-- Module      : Amazonka.CodeBuild.Types.CacheMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.CacheMode
  ( CacheMode
      ( ..,
        CacheMode_LOCAL_CUSTOM_CACHE,
        CacheMode_LOCAL_DOCKER_LAYER_CACHE,
        CacheMode_LOCAL_SOURCE_CACHE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CacheMode = CacheMode'
  { fromCacheMode ::
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
