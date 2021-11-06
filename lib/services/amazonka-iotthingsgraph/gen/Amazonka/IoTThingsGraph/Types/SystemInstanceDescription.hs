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
-- Module      : Amazonka.IoTThingsGraph.Types.SystemInstanceDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.SystemInstanceDescription where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types.DefinitionDocument
import Amazonka.IoTThingsGraph.Types.DependencyRevision
import Amazonka.IoTThingsGraph.Types.MetricsConfiguration
import Amazonka.IoTThingsGraph.Types.SystemInstanceSummary
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains a system instance definition and summary
-- information.
--
-- /See:/ 'newSystemInstanceDescription' smart constructor.
data SystemInstanceDescription = SystemInstanceDescription'
  { -- | An object that contains summary information about a system instance.
    summary :: Prelude.Maybe SystemInstanceSummary,
    metricsConfiguration :: Prelude.Maybe MetricsConfiguration,
    -- | A list of objects that contain all of the IDs and revision numbers of
    -- workflows and systems that are used in a system instance.
    validatedDependencyRevisions :: Prelude.Maybe [DependencyRevision],
    definition :: Prelude.Maybe DefinitionDocument,
    -- | The version of the user\'s namespace against which the system instance
    -- was validated.
    validatedNamespaceVersion :: Prelude.Maybe Prelude.Integer,
    -- | The AWS Identity and Access Management (IAM) role that AWS IoT Things
    -- Graph assumes during flow execution in a cloud deployment. This role
    -- must have read and write permissionss to AWS Lambda and AWS IoT and to
    -- any other AWS services that the flow uses.
    flowActionsRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Simple Storage Service bucket where information about a
    -- system instance is stored.
    s3BucketName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SystemInstanceDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summary', 'systemInstanceDescription_summary' - An object that contains summary information about a system instance.
--
-- 'metricsConfiguration', 'systemInstanceDescription_metricsConfiguration' - Undocumented member.
--
-- 'validatedDependencyRevisions', 'systemInstanceDescription_validatedDependencyRevisions' - A list of objects that contain all of the IDs and revision numbers of
-- workflows and systems that are used in a system instance.
--
-- 'definition', 'systemInstanceDescription_definition' - Undocumented member.
--
-- 'validatedNamespaceVersion', 'systemInstanceDescription_validatedNamespaceVersion' - The version of the user\'s namespace against which the system instance
-- was validated.
--
-- 'flowActionsRoleArn', 'systemInstanceDescription_flowActionsRoleArn' - The AWS Identity and Access Management (IAM) role that AWS IoT Things
-- Graph assumes during flow execution in a cloud deployment. This role
-- must have read and write permissionss to AWS Lambda and AWS IoT and to
-- any other AWS services that the flow uses.
--
-- 's3BucketName', 'systemInstanceDescription_s3BucketName' - The Amazon Simple Storage Service bucket where information about a
-- system instance is stored.
newSystemInstanceDescription ::
  SystemInstanceDescription
newSystemInstanceDescription =
  SystemInstanceDescription'
    { summary =
        Prelude.Nothing,
      metricsConfiguration = Prelude.Nothing,
      validatedDependencyRevisions = Prelude.Nothing,
      definition = Prelude.Nothing,
      validatedNamespaceVersion = Prelude.Nothing,
      flowActionsRoleArn = Prelude.Nothing,
      s3BucketName = Prelude.Nothing
    }

-- | An object that contains summary information about a system instance.
systemInstanceDescription_summary :: Lens.Lens' SystemInstanceDescription (Prelude.Maybe SystemInstanceSummary)
systemInstanceDescription_summary = Lens.lens (\SystemInstanceDescription' {summary} -> summary) (\s@SystemInstanceDescription' {} a -> s {summary = a} :: SystemInstanceDescription)

-- | Undocumented member.
systemInstanceDescription_metricsConfiguration :: Lens.Lens' SystemInstanceDescription (Prelude.Maybe MetricsConfiguration)
systemInstanceDescription_metricsConfiguration = Lens.lens (\SystemInstanceDescription' {metricsConfiguration} -> metricsConfiguration) (\s@SystemInstanceDescription' {} a -> s {metricsConfiguration = a} :: SystemInstanceDescription)

-- | A list of objects that contain all of the IDs and revision numbers of
-- workflows and systems that are used in a system instance.
systemInstanceDescription_validatedDependencyRevisions :: Lens.Lens' SystemInstanceDescription (Prelude.Maybe [DependencyRevision])
systemInstanceDescription_validatedDependencyRevisions = Lens.lens (\SystemInstanceDescription' {validatedDependencyRevisions} -> validatedDependencyRevisions) (\s@SystemInstanceDescription' {} a -> s {validatedDependencyRevisions = a} :: SystemInstanceDescription) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
systemInstanceDescription_definition :: Lens.Lens' SystemInstanceDescription (Prelude.Maybe DefinitionDocument)
systemInstanceDescription_definition = Lens.lens (\SystemInstanceDescription' {definition} -> definition) (\s@SystemInstanceDescription' {} a -> s {definition = a} :: SystemInstanceDescription)

-- | The version of the user\'s namespace against which the system instance
-- was validated.
systemInstanceDescription_validatedNamespaceVersion :: Lens.Lens' SystemInstanceDescription (Prelude.Maybe Prelude.Integer)
systemInstanceDescription_validatedNamespaceVersion = Lens.lens (\SystemInstanceDescription' {validatedNamespaceVersion} -> validatedNamespaceVersion) (\s@SystemInstanceDescription' {} a -> s {validatedNamespaceVersion = a} :: SystemInstanceDescription)

-- | The AWS Identity and Access Management (IAM) role that AWS IoT Things
-- Graph assumes during flow execution in a cloud deployment. This role
-- must have read and write permissionss to AWS Lambda and AWS IoT and to
-- any other AWS services that the flow uses.
systemInstanceDescription_flowActionsRoleArn :: Lens.Lens' SystemInstanceDescription (Prelude.Maybe Prelude.Text)
systemInstanceDescription_flowActionsRoleArn = Lens.lens (\SystemInstanceDescription' {flowActionsRoleArn} -> flowActionsRoleArn) (\s@SystemInstanceDescription' {} a -> s {flowActionsRoleArn = a} :: SystemInstanceDescription)

-- | The Amazon Simple Storage Service bucket where information about a
-- system instance is stored.
systemInstanceDescription_s3BucketName :: Lens.Lens' SystemInstanceDescription (Prelude.Maybe Prelude.Text)
systemInstanceDescription_s3BucketName = Lens.lens (\SystemInstanceDescription' {s3BucketName} -> s3BucketName) (\s@SystemInstanceDescription' {} a -> s {s3BucketName = a} :: SystemInstanceDescription)

instance Core.FromJSON SystemInstanceDescription where
  parseJSON =
    Core.withObject
      "SystemInstanceDescription"
      ( \x ->
          SystemInstanceDescription'
            Prelude.<$> (x Core..:? "summary")
            Prelude.<*> (x Core..:? "metricsConfiguration")
            Prelude.<*> ( x Core..:? "validatedDependencyRevisions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "definition")
            Prelude.<*> (x Core..:? "validatedNamespaceVersion")
            Prelude.<*> (x Core..:? "flowActionsRoleArn")
            Prelude.<*> (x Core..:? "s3BucketName")
      )

instance Prelude.Hashable SystemInstanceDescription

instance Prelude.NFData SystemInstanceDescription
