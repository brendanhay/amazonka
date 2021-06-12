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
-- Module      : Network.AWS.Config.Types.ComplianceByResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ComplianceByResource where

import Network.AWS.Config.Types.Compliance
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Indicates whether an AWS resource that is evaluated according to one or
-- more AWS Config rules is compliant. A resource is compliant if it
-- complies with all of the rules that evaluate it. A resource is
-- noncompliant if it does not comply with one or more of these rules.
--
-- /See:/ 'newComplianceByResource' smart constructor.
data ComplianceByResource = ComplianceByResource'
  { -- | The ID of the AWS resource that was evaluated.
    resourceId :: Core.Maybe Core.Text,
    -- | The type of the AWS resource that was evaluated.
    resourceType :: Core.Maybe Core.Text,
    -- | Indicates whether the AWS resource complies with all of the AWS Config
    -- rules that evaluated it.
    compliance :: Core.Maybe Compliance
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ComplianceByResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'complianceByResource_resourceId' - The ID of the AWS resource that was evaluated.
--
-- 'resourceType', 'complianceByResource_resourceType' - The type of the AWS resource that was evaluated.
--
-- 'compliance', 'complianceByResource_compliance' - Indicates whether the AWS resource complies with all of the AWS Config
-- rules that evaluated it.
newComplianceByResource ::
  ComplianceByResource
newComplianceByResource =
  ComplianceByResource'
    { resourceId = Core.Nothing,
      resourceType = Core.Nothing,
      compliance = Core.Nothing
    }

-- | The ID of the AWS resource that was evaluated.
complianceByResource_resourceId :: Lens.Lens' ComplianceByResource (Core.Maybe Core.Text)
complianceByResource_resourceId = Lens.lens (\ComplianceByResource' {resourceId} -> resourceId) (\s@ComplianceByResource' {} a -> s {resourceId = a} :: ComplianceByResource)

-- | The type of the AWS resource that was evaluated.
complianceByResource_resourceType :: Lens.Lens' ComplianceByResource (Core.Maybe Core.Text)
complianceByResource_resourceType = Lens.lens (\ComplianceByResource' {resourceType} -> resourceType) (\s@ComplianceByResource' {} a -> s {resourceType = a} :: ComplianceByResource)

-- | Indicates whether the AWS resource complies with all of the AWS Config
-- rules that evaluated it.
complianceByResource_compliance :: Lens.Lens' ComplianceByResource (Core.Maybe Compliance)
complianceByResource_compliance = Lens.lens (\ComplianceByResource' {compliance} -> compliance) (\s@ComplianceByResource' {} a -> s {compliance = a} :: ComplianceByResource)

instance Core.FromJSON ComplianceByResource where
  parseJSON =
    Core.withObject
      "ComplianceByResource"
      ( \x ->
          ComplianceByResource'
            Core.<$> (x Core..:? "ResourceId")
            Core.<*> (x Core..:? "ResourceType")
            Core.<*> (x Core..:? "Compliance")
      )

instance Core.Hashable ComplianceByResource

instance Core.NFData ComplianceByResource
