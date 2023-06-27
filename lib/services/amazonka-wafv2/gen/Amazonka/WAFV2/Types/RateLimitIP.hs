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
-- Module      : Amazonka.WAFV2.Types.RateLimitIP
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RateLimitIP where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the IP address in the web request as an aggregate key for a
-- rate-based rule. Each distinct IP address contributes to the aggregation
-- instance.
--
-- This setting is used only in the @RateBasedStatementCustomKey@
-- specification of a rate-based rule statement. To use this in the custom
-- key settings, you must specify at least one other key to use, along with
-- the IP address. To aggregate on only the IP address, in your rate-based
-- statement\'s @AggregateKeyType@, specify @IP@.
--
-- JSON specification: @\"RateLimitIP\": {}@
--
-- /See:/ 'newRateLimitIP' smart constructor.
data RateLimitIP = RateLimitIP'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RateLimitIP' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRateLimitIP ::
  RateLimitIP
newRateLimitIP = RateLimitIP'

instance Data.FromJSON RateLimitIP where
  parseJSON =
    Data.withObject
      "RateLimitIP"
      (\x -> Prelude.pure RateLimitIP')

instance Prelude.Hashable RateLimitIP where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData RateLimitIP where
  rnf _ = ()

instance Data.ToJSON RateLimitIP where
  toJSON = Prelude.const (Data.Object Prelude.mempty)
