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
-- Module      : Amazonka.Connect.Types.IntegrationAssociationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.IntegrationAssociationSummary where

import Amazonka.Connect.Types.IntegrationType
import Amazonka.Connect.Types.SourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about the associated AppIntegrations.
--
-- /See:/ 'newIntegrationAssociationSummary' smart constructor.
data IntegrationAssociationSummary = IntegrationAssociationSummary'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the source.
    sourceType :: Prelude.Maybe SourceType,
    -- | The URL for the external application.
    sourceApplicationUrl :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the AppIntegration association.
    integrationAssociationId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the AppIntegration association.
    integrationAssociationArn :: Prelude.Maybe Prelude.Text,
    -- | The user-provided, friendly name for the external application.
    sourceApplicationName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the AppIntegration.
    integrationArn :: Prelude.Maybe Prelude.Text,
    -- | The integration type.
    integrationType :: Prelude.Maybe IntegrationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntegrationAssociationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'integrationAssociationSummary_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'sourceType', 'integrationAssociationSummary_sourceType' - The name of the source.
--
-- 'sourceApplicationUrl', 'integrationAssociationSummary_sourceApplicationUrl' - The URL for the external application.
--
-- 'integrationAssociationId', 'integrationAssociationSummary_integrationAssociationId' - The identifier for the AppIntegration association.
--
-- 'integrationAssociationArn', 'integrationAssociationSummary_integrationAssociationArn' - The Amazon Resource Name (ARN) for the AppIntegration association.
--
-- 'sourceApplicationName', 'integrationAssociationSummary_sourceApplicationName' - The user-provided, friendly name for the external application.
--
-- 'integrationArn', 'integrationAssociationSummary_integrationArn' - The Amazon Resource Name (ARN) for the AppIntegration.
--
-- 'integrationType', 'integrationAssociationSummary_integrationType' - The integration type.
newIntegrationAssociationSummary ::
  IntegrationAssociationSummary
newIntegrationAssociationSummary =
  IntegrationAssociationSummary'
    { instanceId =
        Prelude.Nothing,
      sourceType = Prelude.Nothing,
      sourceApplicationUrl = Prelude.Nothing,
      integrationAssociationId = Prelude.Nothing,
      integrationAssociationArn = Prelude.Nothing,
      sourceApplicationName = Prelude.Nothing,
      integrationArn = Prelude.Nothing,
      integrationType = Prelude.Nothing
    }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
integrationAssociationSummary_instanceId :: Lens.Lens' IntegrationAssociationSummary (Prelude.Maybe Prelude.Text)
integrationAssociationSummary_instanceId = Lens.lens (\IntegrationAssociationSummary' {instanceId} -> instanceId) (\s@IntegrationAssociationSummary' {} a -> s {instanceId = a} :: IntegrationAssociationSummary)

-- | The name of the source.
integrationAssociationSummary_sourceType :: Lens.Lens' IntegrationAssociationSummary (Prelude.Maybe SourceType)
integrationAssociationSummary_sourceType = Lens.lens (\IntegrationAssociationSummary' {sourceType} -> sourceType) (\s@IntegrationAssociationSummary' {} a -> s {sourceType = a} :: IntegrationAssociationSummary)

-- | The URL for the external application.
integrationAssociationSummary_sourceApplicationUrl :: Lens.Lens' IntegrationAssociationSummary (Prelude.Maybe Prelude.Text)
integrationAssociationSummary_sourceApplicationUrl = Lens.lens (\IntegrationAssociationSummary' {sourceApplicationUrl} -> sourceApplicationUrl) (\s@IntegrationAssociationSummary' {} a -> s {sourceApplicationUrl = a} :: IntegrationAssociationSummary)

-- | The identifier for the AppIntegration association.
integrationAssociationSummary_integrationAssociationId :: Lens.Lens' IntegrationAssociationSummary (Prelude.Maybe Prelude.Text)
integrationAssociationSummary_integrationAssociationId = Lens.lens (\IntegrationAssociationSummary' {integrationAssociationId} -> integrationAssociationId) (\s@IntegrationAssociationSummary' {} a -> s {integrationAssociationId = a} :: IntegrationAssociationSummary)

-- | The Amazon Resource Name (ARN) for the AppIntegration association.
integrationAssociationSummary_integrationAssociationArn :: Lens.Lens' IntegrationAssociationSummary (Prelude.Maybe Prelude.Text)
integrationAssociationSummary_integrationAssociationArn = Lens.lens (\IntegrationAssociationSummary' {integrationAssociationArn} -> integrationAssociationArn) (\s@IntegrationAssociationSummary' {} a -> s {integrationAssociationArn = a} :: IntegrationAssociationSummary)

-- | The user-provided, friendly name for the external application.
integrationAssociationSummary_sourceApplicationName :: Lens.Lens' IntegrationAssociationSummary (Prelude.Maybe Prelude.Text)
integrationAssociationSummary_sourceApplicationName = Lens.lens (\IntegrationAssociationSummary' {sourceApplicationName} -> sourceApplicationName) (\s@IntegrationAssociationSummary' {} a -> s {sourceApplicationName = a} :: IntegrationAssociationSummary)

-- | The Amazon Resource Name (ARN) for the AppIntegration.
integrationAssociationSummary_integrationArn :: Lens.Lens' IntegrationAssociationSummary (Prelude.Maybe Prelude.Text)
integrationAssociationSummary_integrationArn = Lens.lens (\IntegrationAssociationSummary' {integrationArn} -> integrationArn) (\s@IntegrationAssociationSummary' {} a -> s {integrationArn = a} :: IntegrationAssociationSummary)

-- | The integration type.
integrationAssociationSummary_integrationType :: Lens.Lens' IntegrationAssociationSummary (Prelude.Maybe IntegrationType)
integrationAssociationSummary_integrationType = Lens.lens (\IntegrationAssociationSummary' {integrationType} -> integrationType) (\s@IntegrationAssociationSummary' {} a -> s {integrationType = a} :: IntegrationAssociationSummary)

instance Core.FromJSON IntegrationAssociationSummary where
  parseJSON =
    Core.withObject
      "IntegrationAssociationSummary"
      ( \x ->
          IntegrationAssociationSummary'
            Prelude.<$> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "SourceType")
            Prelude.<*> (x Core..:? "SourceApplicationUrl")
            Prelude.<*> (x Core..:? "IntegrationAssociationId")
            Prelude.<*> (x Core..:? "IntegrationAssociationArn")
            Prelude.<*> (x Core..:? "SourceApplicationName")
            Prelude.<*> (x Core..:? "IntegrationArn")
            Prelude.<*> (x Core..:? "IntegrationType")
      )

instance
  Prelude.Hashable
    IntegrationAssociationSummary
  where
  hashWithSalt salt' IntegrationAssociationSummary' {..} =
    salt' `Prelude.hashWithSalt` integrationType
      `Prelude.hashWithSalt` integrationArn
      `Prelude.hashWithSalt` sourceApplicationName
      `Prelude.hashWithSalt` integrationAssociationArn
      `Prelude.hashWithSalt` integrationAssociationId
      `Prelude.hashWithSalt` sourceApplicationUrl
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData IntegrationAssociationSummary where
  rnf IntegrationAssociationSummary' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf integrationType
      `Prelude.seq` Prelude.rnf integrationArn
      `Prelude.seq` Prelude.rnf sourceApplicationName
      `Prelude.seq` Prelude.rnf integrationAssociationArn
      `Prelude.seq` Prelude.rnf integrationAssociationId
      `Prelude.seq` Prelude.rnf sourceApplicationUrl
      `Prelude.seq` Prelude.rnf sourceType
