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
-- Module      : Amazonka.APIGateway.Types.UnauthorizedCacheControlHeaderStrategy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.UnauthorizedCacheControlHeaderStrategy
  ( UnauthorizedCacheControlHeaderStrategy
      ( ..,
        UnauthorizedCacheControlHeaderStrategy_FAIL_WITH_403,
        UnauthorizedCacheControlHeaderStrategy_SUCCEED_WITHOUT_RESPONSE_HEADER,
        UnauthorizedCacheControlHeaderStrategy_SUCCEED_WITH_RESPONSE_HEADER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UnauthorizedCacheControlHeaderStrategy = UnauthorizedCacheControlHeaderStrategy'
  { fromUnauthorizedCacheControlHeaderStrategy ::
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
