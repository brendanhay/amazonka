-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.UnauthorizedCacheControlHeaderStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.UnauthorizedCacheControlHeaderStrategy
  ( UnauthorizedCacheControlHeaderStrategy
      ( UnauthorizedCacheControlHeaderStrategy',
        FailWith403,
        SucceedWithResponseHeader,
        SucceedWithoutResponseHeader
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype UnauthorizedCacheControlHeaderStrategy = UnauthorizedCacheControlHeaderStrategy' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern FailWith403 :: UnauthorizedCacheControlHeaderStrategy
pattern FailWith403 = UnauthorizedCacheControlHeaderStrategy' "FAIL_WITH_403"

pattern SucceedWithResponseHeader :: UnauthorizedCacheControlHeaderStrategy
pattern SucceedWithResponseHeader = UnauthorizedCacheControlHeaderStrategy' "SUCCEED_WITH_RESPONSE_HEADER"

pattern SucceedWithoutResponseHeader :: UnauthorizedCacheControlHeaderStrategy
pattern SucceedWithoutResponseHeader = UnauthorizedCacheControlHeaderStrategy' "SUCCEED_WITHOUT_RESPONSE_HEADER"

{-# COMPLETE
  FailWith403,
  SucceedWithResponseHeader,
  SucceedWithoutResponseHeader,
  UnauthorizedCacheControlHeaderStrategy'
  #-}
