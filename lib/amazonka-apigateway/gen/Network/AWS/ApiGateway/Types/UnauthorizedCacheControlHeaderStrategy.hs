{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.UnauthorizedCacheControlHeaderStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.UnauthorizedCacheControlHeaderStrategy
  ( UnauthorizedCacheControlHeaderStrategy
    ( UnauthorizedCacheControlHeaderStrategy'
    , UnauthorizedCacheControlHeaderStrategyFailWith403
    , UnauthorizedCacheControlHeaderStrategySucceedWithResponseHeader
    , UnauthorizedCacheControlHeaderStrategySucceedWithoutResponseHeader
    , fromUnauthorizedCacheControlHeaderStrategy
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype UnauthorizedCacheControlHeaderStrategy = UnauthorizedCacheControlHeaderStrategy'{fromUnauthorizedCacheControlHeaderStrategy
                                                                                         ::
                                                                                         Core.Text}
                                                   deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                                   Core.Show, Core.Generic)
                                                   deriving newtype (Core.IsString, Core.Hashable,
                                                                     Core.NFData, Core.ToJSONKey,
                                                                     Core.FromJSONKey, Core.ToJSON,
                                                                     Core.FromJSON, Core.ToXML,
                                                                     Core.FromXML, Core.ToText,
                                                                     Core.FromText,
                                                                     Core.ToByteString,
                                                                     Core.ToQuery, Core.ToHeader)

pattern UnauthorizedCacheControlHeaderStrategyFailWith403 :: UnauthorizedCacheControlHeaderStrategy
pattern UnauthorizedCacheControlHeaderStrategyFailWith403 = UnauthorizedCacheControlHeaderStrategy' "FAIL_WITH_403"

pattern UnauthorizedCacheControlHeaderStrategySucceedWithResponseHeader :: UnauthorizedCacheControlHeaderStrategy
pattern UnauthorizedCacheControlHeaderStrategySucceedWithResponseHeader = UnauthorizedCacheControlHeaderStrategy' "SUCCEED_WITH_RESPONSE_HEADER"

pattern UnauthorizedCacheControlHeaderStrategySucceedWithoutResponseHeader :: UnauthorizedCacheControlHeaderStrategy
pattern UnauthorizedCacheControlHeaderStrategySucceedWithoutResponseHeader = UnauthorizedCacheControlHeaderStrategy' "SUCCEED_WITHOUT_RESPONSE_HEADER"

{-# COMPLETE 
  UnauthorizedCacheControlHeaderStrategyFailWith403,

  UnauthorizedCacheControlHeaderStrategySucceedWithResponseHeader,

  UnauthorizedCacheControlHeaderStrategySucceedWithoutResponseHeader,
  UnauthorizedCacheControlHeaderStrategy'
  #-}
