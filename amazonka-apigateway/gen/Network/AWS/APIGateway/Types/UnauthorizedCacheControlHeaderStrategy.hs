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

import qualified Network.AWS.Prelude as Prelude

newtype UnauthorizedCacheControlHeaderStrategy = UnauthorizedCacheControlHeaderStrategy'
  { fromUnauthorizedCacheControlHeaderStrategy ::
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
