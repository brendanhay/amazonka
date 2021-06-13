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
-- Module      : Network.AWS.AppSync.Types.ApiCachingBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.ApiCachingBehavior
  ( ApiCachingBehavior
      ( ..,
        ApiCachingBehavior_FULL_REQUEST_CACHING,
        ApiCachingBehavior_PER_RESOLVER_CACHING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ApiCachingBehavior = ApiCachingBehavior'
  { fromApiCachingBehavior ::
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

pattern ApiCachingBehavior_FULL_REQUEST_CACHING :: ApiCachingBehavior
pattern ApiCachingBehavior_FULL_REQUEST_CACHING = ApiCachingBehavior' "FULL_REQUEST_CACHING"

pattern ApiCachingBehavior_PER_RESOLVER_CACHING :: ApiCachingBehavior
pattern ApiCachingBehavior_PER_RESOLVER_CACHING = ApiCachingBehavior' "PER_RESOLVER_CACHING"

{-# COMPLETE
  ApiCachingBehavior_FULL_REQUEST_CACHING,
  ApiCachingBehavior_PER_RESOLVER_CACHING,
  ApiCachingBehavior'
  #-}
