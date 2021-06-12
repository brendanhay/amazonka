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
-- Module      : Network.AWS.APIGateway.Types.UnauthorizedCacheControlHeaderStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.UnauthorizedCacheControlHeaderStrategy
  ( UnauthorizedCacheControlHeaderStrategy
      ( ..,
        UnauthorizedCacheControlHeaderStrategy_FAIL_WITH_403,
        UnauthorizedCacheControlHeaderStrategy_SUCCEED_WITHOUT_RESPONSE_HEADER,
        UnauthorizedCacheControlHeaderStrategy_SUCCEED_WITH_RESPONSE_HEADER
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype UnauthorizedCacheControlHeaderStrategy = UnauthorizedCacheControlHeaderStrategy'
  { fromUnauthorizedCacheControlHeaderStrategy ::
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

pattern UnauthorizedCacheControlHeaderStrategy_FAIL_WITH_403 :: UnauthorizedCacheControlHeaderStrategy
pattern UnauthorizedCacheControlHeaderStrategy_FAIL_WITH_403 = UnauthorizedCacheControlHeaderStrategy' "FAIL_WITH_403"

pattern UnauthorizedCacheControlHeaderStrategy_SUCCEED_WITHOUT_RESPONSE_HEADER :: UnauthorizedCacheControlHeaderStrategy
pattern UnauthorizedCacheControlHeaderStrategy_SUCCEED_WITHOUT_RESPONSE_HEADER = UnauthorizedCacheControlHeaderStrategy' "SUCCEED_WITHOUT_RESPONSE_HEADER"

pattern UnauthorizedCacheControlHeaderStrategy_SUCCEED_WITH_RESPONSE_HEADER :: UnauthorizedCacheControlHeaderStrategy
pattern UnauthorizedCacheControlHeaderStrategy_SUCCEED_WITH_RESPONSE_HEADER = UnauthorizedCacheControlHeaderStrategy' "SUCCEED_WITH_RESPONSE_HEADER"

{-# COMPLETE
  UnauthorizedCacheControlHeaderStrategy_FAIL_WITH_403,
  UnauthorizedCacheControlHeaderStrategy_SUCCEED_WITHOUT_RESPONSE_HEADER,
  UnauthorizedCacheControlHeaderStrategy_SUCCEED_WITH_RESPONSE_HEADER,
  UnauthorizedCacheControlHeaderStrategy'
  #-}
