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
-- Module      : Network.AWS.Proton.Types.ServiceTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Proton.Types.ServiceTemplate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Proton.Types.Provisioning

-- | The service template detail data.
--
-- /See:/ 'newServiceTemplate' smart constructor.
data ServiceTemplate = ServiceTemplate'
  { -- | The ID of the recommended version of the service template.
    recommendedVersion :: Prelude.Maybe Prelude.Text,
    -- | The service template name as displayed in the developer interface.
    displayName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The customer provided service template encryption key that\'s used to
    -- encrypt data.
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | If @pipelineProvisioning@ is @true@, a service pipeline is included in
    -- the service template. Otherwise, a service pipeline /isn\'t/ included in
    -- the service template.
    pipelineProvisioning :: Prelude.Maybe Provisioning,
    -- | A description of the service template.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the service template.
    arn :: Prelude.Text,
    -- | The time when the service template was created.
    createdAt :: Core.POSIX,
    -- | The time when the service template was last modified.
    lastModifiedAt :: Core.POSIX,
    -- | The name of the service template.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendedVersion', 'serviceTemplate_recommendedVersion' - The ID of the recommended version of the service template.
--
-- 'displayName', 'serviceTemplate_displayName' - The service template name as displayed in the developer interface.
--
-- 'encryptionKey', 'serviceTemplate_encryptionKey' - The customer provided service template encryption key that\'s used to
-- encrypt data.
--
-- 'pipelineProvisioning', 'serviceTemplate_pipelineProvisioning' - If @pipelineProvisioning@ is @true@, a service pipeline is included in
-- the service template. Otherwise, a service pipeline /isn\'t/ included in
-- the service template.
--
-- 'description', 'serviceTemplate_description' - A description of the service template.
--
-- 'arn', 'serviceTemplate_arn' - The Amazon Resource Name (ARN) of the service template.
--
-- 'createdAt', 'serviceTemplate_createdAt' - The time when the service template was created.
--
-- 'lastModifiedAt', 'serviceTemplate_lastModifiedAt' - The time when the service template was last modified.
--
-- 'name', 'serviceTemplate_name' - The name of the service template.
newServiceTemplate ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastModifiedAt'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  ServiceTemplate
newServiceTemplate
  pArn_
  pCreatedAt_
  pLastModifiedAt_
  pName_ =
    ServiceTemplate'
      { recommendedVersion =
          Prelude.Nothing,
        displayName = Prelude.Nothing,
        encryptionKey = Prelude.Nothing,
        pipelineProvisioning = Prelude.Nothing,
        description = Prelude.Nothing,
        arn = pArn_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        lastModifiedAt = Core._Time Lens.# pLastModifiedAt_,
        name = pName_
      }

-- | The ID of the recommended version of the service template.
serviceTemplate_recommendedVersion :: Lens.Lens' ServiceTemplate (Prelude.Maybe Prelude.Text)
serviceTemplate_recommendedVersion = Lens.lens (\ServiceTemplate' {recommendedVersion} -> recommendedVersion) (\s@ServiceTemplate' {} a -> s {recommendedVersion = a} :: ServiceTemplate)

-- | The service template name as displayed in the developer interface.
serviceTemplate_displayName :: Lens.Lens' ServiceTemplate (Prelude.Maybe Prelude.Text)
serviceTemplate_displayName = Lens.lens (\ServiceTemplate' {displayName} -> displayName) (\s@ServiceTemplate' {} a -> s {displayName = a} :: ServiceTemplate) Prelude.. Lens.mapping Core._Sensitive

-- | The customer provided service template encryption key that\'s used to
-- encrypt data.
serviceTemplate_encryptionKey :: Lens.Lens' ServiceTemplate (Prelude.Maybe Prelude.Text)
serviceTemplate_encryptionKey = Lens.lens (\ServiceTemplate' {encryptionKey} -> encryptionKey) (\s@ServiceTemplate' {} a -> s {encryptionKey = a} :: ServiceTemplate)

-- | If @pipelineProvisioning@ is @true@, a service pipeline is included in
-- the service template. Otherwise, a service pipeline /isn\'t/ included in
-- the service template.
serviceTemplate_pipelineProvisioning :: Lens.Lens' ServiceTemplate (Prelude.Maybe Provisioning)
serviceTemplate_pipelineProvisioning = Lens.lens (\ServiceTemplate' {pipelineProvisioning} -> pipelineProvisioning) (\s@ServiceTemplate' {} a -> s {pipelineProvisioning = a} :: ServiceTemplate)

-- | A description of the service template.
serviceTemplate_description :: Lens.Lens' ServiceTemplate (Prelude.Maybe Prelude.Text)
serviceTemplate_description = Lens.lens (\ServiceTemplate' {description} -> description) (\s@ServiceTemplate' {} a -> s {description = a} :: ServiceTemplate) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) of the service template.
serviceTemplate_arn :: Lens.Lens' ServiceTemplate Prelude.Text
serviceTemplate_arn = Lens.lens (\ServiceTemplate' {arn} -> arn) (\s@ServiceTemplate' {} a -> s {arn = a} :: ServiceTemplate)

-- | The time when the service template was created.
serviceTemplate_createdAt :: Lens.Lens' ServiceTemplate Prelude.UTCTime
serviceTemplate_createdAt = Lens.lens (\ServiceTemplate' {createdAt} -> createdAt) (\s@ServiceTemplate' {} a -> s {createdAt = a} :: ServiceTemplate) Prelude.. Core._Time

-- | The time when the service template was last modified.
serviceTemplate_lastModifiedAt :: Lens.Lens' ServiceTemplate Prelude.UTCTime
serviceTemplate_lastModifiedAt = Lens.lens (\ServiceTemplate' {lastModifiedAt} -> lastModifiedAt) (\s@ServiceTemplate' {} a -> s {lastModifiedAt = a} :: ServiceTemplate) Prelude.. Core._Time

-- | The name of the service template.
serviceTemplate_name :: Lens.Lens' ServiceTemplate Prelude.Text
serviceTemplate_name = Lens.lens (\ServiceTemplate' {name} -> name) (\s@ServiceTemplate' {} a -> s {name = a} :: ServiceTemplate)

instance Core.FromJSON ServiceTemplate where
  parseJSON =
    Core.withObject
      "ServiceTemplate"
      ( \x ->
          ServiceTemplate'
            Prelude.<$> (x Core..:? "recommendedVersion")
            Prelude.<*> (x Core..:? "displayName")
            Prelude.<*> (x Core..:? "encryptionKey")
            Prelude.<*> (x Core..:? "pipelineProvisioning")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..: "arn")
            Prelude.<*> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "lastModifiedAt")
            Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable ServiceTemplate

instance Prelude.NFData ServiceTemplate
