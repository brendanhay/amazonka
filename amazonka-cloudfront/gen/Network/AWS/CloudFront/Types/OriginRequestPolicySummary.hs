{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicySummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicySummary where

import Network.AWS.CloudFront.Types.OriginRequestPolicy
import Network.AWS.CloudFront.Types.OriginRequestPolicyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains an origin request policy.
--
-- /See:/ 'newOriginRequestPolicySummary' smart constructor.
data OriginRequestPolicySummary = OriginRequestPolicySummary'
  { -- | The type of origin request policy, either @managed@ (created by AWS) or
    -- @custom@ (created in this AWS account).
    type' :: OriginRequestPolicyType,
    -- | The origin request policy.
    originRequestPolicy :: OriginRequestPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OriginRequestPolicySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'originRequestPolicySummary_type' - The type of origin request policy, either @managed@ (created by AWS) or
-- @custom@ (created in this AWS account).
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

-- | The type of origin request policy, either @managed@ (created by AWS) or
-- @custom@ (created in this AWS account).
originRequestPolicySummary_type :: Lens.Lens' OriginRequestPolicySummary OriginRequestPolicyType
originRequestPolicySummary_type = Lens.lens (\OriginRequestPolicySummary' {type'} -> type') (\s@OriginRequestPolicySummary' {} a -> s {type' = a} :: OriginRequestPolicySummary)

-- | The origin request policy.
originRequestPolicySummary_originRequestPolicy :: Lens.Lens' OriginRequestPolicySummary OriginRequestPolicy
originRequestPolicySummary_originRequestPolicy = Lens.lens (\OriginRequestPolicySummary' {originRequestPolicy} -> originRequestPolicy) (\s@OriginRequestPolicySummary' {} a -> s {originRequestPolicy = a} :: OriginRequestPolicySummary)

instance Prelude.FromXML OriginRequestPolicySummary where
  parseXML x =
    OriginRequestPolicySummary'
      Prelude.<$> (x Prelude..@ "Type")
      Prelude.<*> (x Prelude..@ "OriginRequestPolicy")

instance Prelude.Hashable OriginRequestPolicySummary

instance Prelude.NFData OriginRequestPolicySummary
