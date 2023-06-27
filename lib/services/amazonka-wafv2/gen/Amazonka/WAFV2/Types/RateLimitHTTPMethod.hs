{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WAFV2.Types.RateLimitHTTPMethod
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RateLimitHTTPMethod where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the request\'s HTTP method as an aggregate key for a
-- rate-based rule. Each distinct HTTP method contributes to the
-- aggregation instance. If you use just the HTTP method as your custom
-- key, then each method fully defines an aggregation instance.
--
-- JSON specification: @\"RateLimitHTTPMethod\": {}@
--
-- /See:/ 'newRateLimitHTTPMethod' smart constructor.
data RateLimitHTTPMethod = RateLimitHTTPMethod'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RateLimitHTTPMethod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRateLimitHTTPMethod ::
  RateLimitHTTPMethod
newRateLimitHTTPMethod = RateLimitHTTPMethod'

instance Data.FromJSON RateLimitHTTPMethod where
  parseJSON =
    Data.withObject
      "RateLimitHTTPMethod"
      (\x -> Prelude.pure RateLimitHTTPMethod')

instance Prelude.Hashable RateLimitHTTPMethod where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData RateLimitHTTPMethod where
  rnf _ = ()

instance Data.ToJSON RateLimitHTTPMethod where
  toJSON = Prelude.const (Data.Object Prelude.mempty)
