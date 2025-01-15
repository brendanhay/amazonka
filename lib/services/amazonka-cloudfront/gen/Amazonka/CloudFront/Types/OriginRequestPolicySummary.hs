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
-- Module      : Amazonka.CloudFront.Types.OriginRequestPolicySummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginRequestPolicySummary where

import Amazonka.CloudFront.Types.OriginRequestPolicy
import Amazonka.CloudFront.Types.OriginRequestPolicyType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains an origin request policy.
--
-- /See:/ 'newOriginRequestPolicySummary' smart constructor.
data OriginRequestPolicySummary = OriginRequestPolicySummary'
  { -- | The type of origin request policy, either @managed@ (created by Amazon
    -- Web Services) or @custom@ (created in this Amazon Web Services account).
    type' :: OriginRequestPolicyType,
    -- | The origin request policy.
    originRequestPolicy :: OriginRequestPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OriginRequestPolicySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'originRequestPolicySummary_type' - The type of origin request policy, either @managed@ (created by Amazon
-- Web Services) or @custom@ (created in this Amazon Web Services account).
--
-- 'originRequestPolicy', 'originRequestPolicySummary_originRequestPolicy' - The origin request policy.
newOriginRequestPolicySummary ::
  -- | 'type''
  OriginRequestPolicyType ->
  -- | 'originRequestPolicy'
  OriginRequestPolicy ->
  OriginRequestPolicySummary
newOriginRequestPolicySummary
  pType_
  pOriginRequestPolicy_ =
    OriginRequestPolicySummary'
      { type' = pType_,
        originRequestPolicy = pOriginRequestPolicy_
      }

-- | The type of origin request policy, either @managed@ (created by Amazon
-- Web Services) or @custom@ (created in this Amazon Web Services account).
originRequestPolicySummary_type :: Lens.Lens' OriginRequestPolicySummary OriginRequestPolicyType
originRequestPolicySummary_type = Lens.lens (\OriginRequestPolicySummary' {type'} -> type') (\s@OriginRequestPolicySummary' {} a -> s {type' = a} :: OriginRequestPolicySummary)

-- | The origin request policy.
originRequestPolicySummary_originRequestPolicy :: Lens.Lens' OriginRequestPolicySummary OriginRequestPolicy
originRequestPolicySummary_originRequestPolicy = Lens.lens (\OriginRequestPolicySummary' {originRequestPolicy} -> originRequestPolicy) (\s@OriginRequestPolicySummary' {} a -> s {originRequestPolicy = a} :: OriginRequestPolicySummary)

instance Data.FromXML OriginRequestPolicySummary where
  parseXML x =
    OriginRequestPolicySummary'
      Prelude.<$> (x Data..@ "Type")
      Prelude.<*> (x Data..@ "OriginRequestPolicy")

instance Prelude.Hashable OriginRequestPolicySummary where
  hashWithSalt _salt OriginRequestPolicySummary' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` originRequestPolicy

instance Prelude.NFData OriginRequestPolicySummary where
  rnf OriginRequestPolicySummary' {..} =
    Prelude.rnf type' `Prelude.seq`
      Prelude.rnf originRequestPolicy
