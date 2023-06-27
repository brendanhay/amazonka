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
-- Module      : Amazonka.WAFV2.Types.RateLimitForwardedIP
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RateLimitForwardedIP where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the first IP address in an HTTP header as an aggregate key for
-- a rate-based rule. Each distinct forwarded IP address contributes to the
-- aggregation instance.
--
-- This setting is used only in the @RateBasedStatementCustomKey@
-- specification of a rate-based rule statement. When you specify an IP or
-- forwarded IP in the custom key settings, you must also specify at least
-- one other key to use. You can aggregate on only the forwarded IP address
-- by specifying @FORWARDED_IP@ in your rate-based statement\'s
-- @AggregateKeyType@.
--
-- This data type supports using the forwarded IP address in the web
-- request aggregation for a rate-based rule, in
-- @RateBasedStatementCustomKey@. The JSON specification for using the
-- forwarded IP address doesn\'t explicitly use this data type.
--
-- JSON specification: @\"ForwardedIP\": {}@
--
-- When you use this specification, you must also configure the forwarded
-- IP address in the rate-based statement\'s @ForwardedIPConfig@.
--
-- /See:/ 'newRateLimitForwardedIP' smart constructor.
data RateLimitForwardedIP = RateLimitForwardedIP'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RateLimitForwardedIP' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRateLimitForwardedIP ::
  RateLimitForwardedIP
newRateLimitForwardedIP = RateLimitForwardedIP'

instance Data.FromJSON RateLimitForwardedIP where
  parseJSON =
    Data.withObject
      "RateLimitForwardedIP"
      (\x -> Prelude.pure RateLimitForwardedIP')

instance Prelude.Hashable RateLimitForwardedIP where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData RateLimitForwardedIP where
  rnf _ = ()

instance Data.ToJSON RateLimitForwardedIP where
  toJSON = Prelude.const (Data.Object Prelude.mempty)
