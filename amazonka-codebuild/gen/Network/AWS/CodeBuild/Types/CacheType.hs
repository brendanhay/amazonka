{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.CacheType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.CacheType
  ( CacheType
      ( ..,
        CacheType_LOCAL,
        CacheType_NO_CACHE,
        CacheType_S3
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype CacheType = CacheType'
  { fromCacheType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern CacheType_LOCAL :: CacheType
pattern CacheType_LOCAL = CacheType' "LOCAL"

pattern CacheType_NO_CACHE :: CacheType
pattern CacheType_NO_CACHE = CacheType' "NO_CACHE"

pattern CacheType_S3 :: CacheType
pattern CacheType_S3 = CacheType' "S3"

{-# COMPLETE
  CacheType_LOCAL,
  CacheType_NO_CACHE,
  CacheType_S3,
  CacheType'
  #-}
