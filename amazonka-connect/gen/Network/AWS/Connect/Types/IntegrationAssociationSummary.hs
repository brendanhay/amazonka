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
-- Module      : Network.AWS.Connect.Types.IntegrationAssociationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.IntegrationAssociationSummary where

import Network.AWS.Connect.Types.IntegrationType
import Network.AWS.Connect.Types.SourceType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains summary information about the associated AppIntegrations.
--
-- /See:/ 'newIntegrationAssociationSummary' smart constructor.
data IntegrationAssociationSummary = IntegrationAssociationSummary'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Maybe Core.Text,
    -- | The user-provided, friendly name for the external application.
    sourceApplicationName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) for the AppIntegration association.
    integrationAssociationArn :: Core.Maybe Core.Text,
    -- | The URL for the external application.
    sourceApplicationUrl :: Core.Maybe Core.Text,
    -- | The integration type.
    integrationType :: Core.Maybe IntegrationType,
    -- | The Amazon Resource Name (ARN) for the AppIntegration.
    integrationArn :: Core.Maybe Core.Text,
    -- | The name of the source.
    sourceType :: Core.Maybe SourceType,
    -- | The identifier for the AppIntegration association.
    integrationAssociationId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IntegrationAssociationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'integrationAssociationSummary_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'sourceApplicationName', 'integrationAssociationSummary_sourceApplicationName' - The user-provided, friendly name for the external application.
--
-- 'integrationAssociationArn', 'integrationAssociationSummary_integrationAssociationArn' - The Amazon Resource Name (ARN) for the AppIntegration association.
--
-- 'sourceApplicationUrl', 'integrationAssociationSummary_sourceApplicationUrl' - The URL for the external application.
--
-- 'integrationType', 'integrationAssociationSummary_integrationType' - The integration type.
--
-- 'integrationArn', 'integrationAssociationSummary_integrationArn' - The Amazon Resource Name (ARN) for the AppIntegration.
--
-- 'sourceType', 'integrationAssociationSummary_sourceType' - The name of the source.
--
-- 'integrationAssociationId', 'integrationAssociationSummary_integrationAssociationId' - The identifier for the AppIntegration association.
newIntegrationAssociationSummary ::
  IntegrationAssociationSummary
newIntegrationAssociationSummary =
  IntegrationAssociationSummary'
    { instanceId =
        Core.Nothing,
      sourceApplicationName = Core.Nothing,
      integrationAssociationArn = Core.Nothing,
      sourceApplicationUrl = Core.Nothing,
      integrationType = Core.Nothing,
      integrationArn = Core.Nothing,
      sourceType = Core.Nothing,
      integrationAssociationId = Core.Nothing
    }

-- | The identifier of the Amazon Connect instance.
integrationAssociationSummary_instanceId :: Lens.Lens' IntegrationAssociationSummary (Core.Maybe Core.Text)
integrationAssociationSummary_instanceId = Lens.lens (\IntegrationAssociationSummary' {instanceId} -> instanceId) (\s@IntegrationAssociationSummary' {} a -> s {instanceId = a} :: IntegrationAssociationSummary)

-- | The user-provided, friendly name for the external application.
integrationAssociationSummary_sourceApplicationName :: Lens.Lens' IntegrationAssociationSummary (Core.Maybe Core.Text)
integrationAssociationSummary_sourceApplicationName = Lens.lens (\IntegrationAssociationSummary' {sourceApplicationName} -> sourceApplicationName) (\s@IntegrationAssociationSummary' {} a -> s {sourceApplicationName = a} :: IntegrationAssociationSummary)

-- | The Amazon Resource Name (ARN) for the AppIntegration association.
integrationAssociationSummary_integrationAssociationArn :: Lens.Lens' IntegrationAssociationSummary (Core.Maybe Core.Text)
integrationAssociationSummary_integrationAssociationArn = Lens.lens (\IntegrationAssociationSummary' {integrationAssociationArn} -> integrationAssociationArn) (\s@IntegrationAssociationSummary' {} a -> s {integrationAssociationArn = a} :: IntegrationAssociationSummary)

-- | The URL for the external application.
integrationAssociationSummary_sourceApplicationUrl :: Lens.Lens' IntegrationAssociationSummary (Core.Maybe Core.Text)
integrationAssociationSummary_sourceApplicationUrl = Lens.lens (\IntegrationAssociationSummary' {sourceApplicationUrl} -> sourceApplicationUrl) (\s@IntegrationAssociationSummary' {} a -> s {sourceApplicationUrl = a} :: IntegrationAssociationSummary)

-- | The integration type.
integrationAssociationSummary_integrationType :: Lens.Lens' IntegrationAssociationSummary (Core.Maybe IntegrationType)
integrationAssociationSummary_integrationType = Lens.lens (\IntegrationAssociationSummary' {integrationType} -> integrationType) (\s@IntegrationAssociationSummary' {} a -> s {integrationType = a} :: IntegrationAssociationSummary)

-- | The Amazon Resource Name (ARN) for the AppIntegration.
integrationAssociationSummary_integrationArn :: Lens.Lens' IntegrationAssociationSummary (Core.Maybe Core.Text)
integrationAssociationSummary_integrationArn = Lens.lens (\IntegrationAssociationSummary' {integrationArn} -> integrationArn) (\s@IntegrationAssociationSummary' {} a -> s {integrationArn = a} :: IntegrationAssociationSummary)

-- | The name of the source.
integrationAssociationSummary_sourceType :: Lens.Lens' IntegrationAssociationSummary (Core.Maybe SourceType)
integrationAssociationSummary_sourceType = Lens.lens (\IntegrationAssociationSummary' {sourceType} -> sourceType) (\s@IntegrationAssociationSummary' {} a -> s {sourceType = a} :: IntegrationAssociationSummary)

-- | The identifier for the AppIntegration association.
integrationAssociationSummary_integrationAssociationId :: Lens.Lens' IntegrationAssociationSummary (Core.Maybe Core.Text)
integrationAssociationSummary_integrationAssociationId = Lens.lens (\IntegrationAssociationSummary' {integrationAssociationId} -> integrationAssociationId) (\s@IntegrationAssociationSummary' {} a -> s {integrationAssociationId = a} :: IntegrationAssociationSummary)

instance Core.FromJSON IntegrationAssociationSummary where
  parseJSON =
    Core.withObject
      "IntegrationAssociationSummary"
      ( \x ->
          IntegrationAssociationSummary'
            Core.<$> (x Core..:? "InstanceId")
            Core.<*> (x Core..:? "SourceApplicationName")
            Core.<*> (x Core..:? "IntegrationAssociationArn")
            Core.<*> (x Core..:? "SourceApplicationUrl")
            Core.<*> (x Core..:? "IntegrationType")
            Core.<*> (x Core..:? "IntegrationArn")
            Core.<*> (x Core..:? "SourceType")
            Core.<*> (x Core..:? "IntegrationAssociationId")
      )

instance Core.Hashable IntegrationAssociationSummary

instance Core.NFData IntegrationAssociationSummary
