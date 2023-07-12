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
-- Module      : Amazonka.FMS.Types.ComplianceViolator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.ComplianceViolator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.ViolationReason
import qualified Amazonka.Prelude as Prelude

-- | Details of the resource that is not protected by the policy.
--
-- /See:/ 'newComplianceViolator' smart constructor.
data ComplianceViolator = ComplianceViolator'
  { -- | Metadata about the resource that doesn\'t comply with the policy scope.
    metadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The resource ID.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The resource type. This is in the format shown in the
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services Resource Types Reference>.
    -- For example: @AWS::ElasticLoadBalancingV2::LoadBalancer@,
    -- @AWS::CloudFront::Distribution@, or
    -- @AWS::NetworkFirewall::FirewallPolicy@.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The reason that the resource is not protected by the policy.
    violationReason :: Prelude.Maybe ViolationReason
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComplianceViolator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'complianceViolator_metadata' - Metadata about the resource that doesn\'t comply with the policy scope.
--
-- 'resourceId', 'complianceViolator_resourceId' - The resource ID.
--
-- 'resourceType', 'complianceViolator_resourceType' - The resource type. This is in the format shown in the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services Resource Types Reference>.
-- For example: @AWS::ElasticLoadBalancingV2::LoadBalancer@,
-- @AWS::CloudFront::Distribution@, or
-- @AWS::NetworkFirewall::FirewallPolicy@.
--
-- 'violationReason', 'complianceViolator_violationReason' - The reason that the resource is not protected by the policy.
newComplianceViolator ::
  ComplianceViolator
newComplianceViolator =
  ComplianceViolator'
    { metadata = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      violationReason = Prelude.Nothing
    }

-- | Metadata about the resource that doesn\'t comply with the policy scope.
complianceViolator_metadata :: Lens.Lens' ComplianceViolator (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
complianceViolator_metadata = Lens.lens (\ComplianceViolator' {metadata} -> metadata) (\s@ComplianceViolator' {} a -> s {metadata = a} :: ComplianceViolator) Prelude.. Lens.mapping Lens.coerced

-- | The resource ID.
complianceViolator_resourceId :: Lens.Lens' ComplianceViolator (Prelude.Maybe Prelude.Text)
complianceViolator_resourceId = Lens.lens (\ComplianceViolator' {resourceId} -> resourceId) (\s@ComplianceViolator' {} a -> s {resourceId = a} :: ComplianceViolator)

-- | The resource type. This is in the format shown in the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services Resource Types Reference>.
-- For example: @AWS::ElasticLoadBalancingV2::LoadBalancer@,
-- @AWS::CloudFront::Distribution@, or
-- @AWS::NetworkFirewall::FirewallPolicy@.
complianceViolator_resourceType :: Lens.Lens' ComplianceViolator (Prelude.Maybe Prelude.Text)
complianceViolator_resourceType = Lens.lens (\ComplianceViolator' {resourceType} -> resourceType) (\s@ComplianceViolator' {} a -> s {resourceType = a} :: ComplianceViolator)

-- | The reason that the resource is not protected by the policy.
complianceViolator_violationReason :: Lens.Lens' ComplianceViolator (Prelude.Maybe ViolationReason)
complianceViolator_violationReason = Lens.lens (\ComplianceViolator' {violationReason} -> violationReason) (\s@ComplianceViolator' {} a -> s {violationReason = a} :: ComplianceViolator)

instance Data.FromJSON ComplianceViolator where
  parseJSON =
    Data.withObject
      "ComplianceViolator"
      ( \x ->
          ComplianceViolator'
            Prelude.<$> (x Data..:? "Metadata" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ResourceId")
            Prelude.<*> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "ViolationReason")
      )

instance Prelude.Hashable ComplianceViolator where
  hashWithSalt _salt ComplianceViolator' {..} =
    _salt
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` violationReason

instance Prelude.NFData ComplianceViolator where
  rnf ComplianceViolator' {..} =
    Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf violationReason
