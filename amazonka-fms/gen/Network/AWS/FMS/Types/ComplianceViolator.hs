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
-- Module      : Network.AWS.FMS.Types.ComplianceViolator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ComplianceViolator where

import Network.AWS.FMS.Types.ViolationReason
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details of the resource that is not protected by the policy.
--
-- /See:/ 'newComplianceViolator' smart constructor.
data ComplianceViolator = ComplianceViolator'
  { -- | The resource ID.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The resource type. This is in the format shown in the
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>.
    -- For example: @AWS::ElasticLoadBalancingV2::LoadBalancer@,
    -- @AWS::CloudFront::Distribution@, or
    -- @AWS::NetworkFirewall::FirewallPolicy@.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The reason that the resource is not protected by the policy.
    violationReason :: Prelude.Maybe ViolationReason
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ComplianceViolator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'complianceViolator_resourceId' - The resource ID.
--
-- 'resourceType', 'complianceViolator_resourceType' - The resource type. This is in the format shown in the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>.
-- For example: @AWS::ElasticLoadBalancingV2::LoadBalancer@,
-- @AWS::CloudFront::Distribution@, or
-- @AWS::NetworkFirewall::FirewallPolicy@.
--
-- 'violationReason', 'complianceViolator_violationReason' - The reason that the resource is not protected by the policy.
newComplianceViolator ::
  ComplianceViolator
newComplianceViolator =
  ComplianceViolator'
    { resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      violationReason = Prelude.Nothing
    }

-- | The resource ID.
complianceViolator_resourceId :: Lens.Lens' ComplianceViolator (Prelude.Maybe Prelude.Text)
complianceViolator_resourceId = Lens.lens (\ComplianceViolator' {resourceId} -> resourceId) (\s@ComplianceViolator' {} a -> s {resourceId = a} :: ComplianceViolator)

-- | The resource type. This is in the format shown in the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>.
-- For example: @AWS::ElasticLoadBalancingV2::LoadBalancer@,
-- @AWS::CloudFront::Distribution@, or
-- @AWS::NetworkFirewall::FirewallPolicy@.
complianceViolator_resourceType :: Lens.Lens' ComplianceViolator (Prelude.Maybe Prelude.Text)
complianceViolator_resourceType = Lens.lens (\ComplianceViolator' {resourceType} -> resourceType) (\s@ComplianceViolator' {} a -> s {resourceType = a} :: ComplianceViolator)

-- | The reason that the resource is not protected by the policy.
complianceViolator_violationReason :: Lens.Lens' ComplianceViolator (Prelude.Maybe ViolationReason)
complianceViolator_violationReason = Lens.lens (\ComplianceViolator' {violationReason} -> violationReason) (\s@ComplianceViolator' {} a -> s {violationReason = a} :: ComplianceViolator)

instance Prelude.FromJSON ComplianceViolator where
  parseJSON =
    Prelude.withObject
      "ComplianceViolator"
      ( \x ->
          ComplianceViolator'
            Prelude.<$> (x Prelude..:? "ResourceId")
            Prelude.<*> (x Prelude..:? "ResourceType")
            Prelude.<*> (x Prelude..:? "ViolationReason")
      )

instance Prelude.Hashable ComplianceViolator

instance Prelude.NFData ComplianceViolator
