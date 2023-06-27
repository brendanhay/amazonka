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
-- Module      : Amazonka.WAFV2.Types.RateLimitLabelNamespace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RateLimitLabelNamespace where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a label namespace to use as an aggregate key for a rate-based
-- rule. Each distinct fully qualified label name that has the specified
-- label namespace contributes to the aggregation instance. If you use just
-- one label namespace as your custom key, then each label name fully
-- defines an aggregation instance.
--
-- This uses only labels that have been added to the request by rules that
-- are evaluated before this rate-based rule in the web ACL.
--
-- For information about label namespaces and names, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-rule-label-requirements.html Label syntax and naming requirements>
-- in the /WAF Developer Guide/.
--
-- /See:/ 'newRateLimitLabelNamespace' smart constructor.
data RateLimitLabelNamespace = RateLimitLabelNamespace'
  { -- | The namespace to use for aggregation.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RateLimitLabelNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespace', 'rateLimitLabelNamespace_namespace' - The namespace to use for aggregation.
newRateLimitLabelNamespace ::
  -- | 'namespace'
  Prelude.Text ->
  RateLimitLabelNamespace
newRateLimitLabelNamespace pNamespace_ =
  RateLimitLabelNamespace' {namespace = pNamespace_}

-- | The namespace to use for aggregation.
rateLimitLabelNamespace_namespace :: Lens.Lens' RateLimitLabelNamespace Prelude.Text
rateLimitLabelNamespace_namespace = Lens.lens (\RateLimitLabelNamespace' {namespace} -> namespace) (\s@RateLimitLabelNamespace' {} a -> s {namespace = a} :: RateLimitLabelNamespace)

instance Data.FromJSON RateLimitLabelNamespace where
  parseJSON =
    Data.withObject
      "RateLimitLabelNamespace"
      ( \x ->
          RateLimitLabelNamespace'
            Prelude.<$> (x Data..: "Namespace")
      )

instance Prelude.Hashable RateLimitLabelNamespace where
  hashWithSalt _salt RateLimitLabelNamespace' {..} =
    _salt `Prelude.hashWithSalt` namespace

instance Prelude.NFData RateLimitLabelNamespace where
  rnf RateLimitLabelNamespace' {..} =
    Prelude.rnf namespace

instance Data.ToJSON RateLimitLabelNamespace where
  toJSON RateLimitLabelNamespace' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Namespace" Data..= namespace)]
      )
