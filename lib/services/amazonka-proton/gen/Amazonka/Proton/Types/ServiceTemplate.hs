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
-- Module      : Amazonka.Proton.Types.ServiceTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ServiceTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.Provisioning

-- | Detailed data of an Proton service template resource.
--
-- /See:/ 'newServiceTemplate' smart constructor.
data ServiceTemplate = ServiceTemplate'
  { -- | A description of the service template.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The service template name as displayed in the developer interface.
    displayName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The customer provided service template encryption key that\'s used to
    -- encrypt data.
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | If @pipelineProvisioning@ is @true@, a service pipeline is included in
    -- the service template. Otherwise, a service pipeline /isn\'t/ included in
    -- the service template.
    pipelineProvisioning :: Prelude.Maybe Provisioning,
    -- | The recommended version of the service template.
    recommendedVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the service template.
    arn :: Prelude.Text,
    -- | The time when the service template was created.
    createdAt :: Data.POSIX,
    -- | The time when the service template was last modified.
    lastModifiedAt :: Data.POSIX,
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
-- 'description', 'serviceTemplate_description' - A description of the service template.
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
-- 'recommendedVersion', 'serviceTemplate_recommendedVersion' - The recommended version of the service template.
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
      { description = Prelude.Nothing,
        displayName = Prelude.Nothing,
        encryptionKey = Prelude.Nothing,
        pipelineProvisioning = Prelude.Nothing,
        recommendedVersion = Prelude.Nothing,
        arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        lastModifiedAt = Data._Time Lens.# pLastModifiedAt_,
        name = pName_
      }

-- | A description of the service template.
serviceTemplate_description :: Lens.Lens' ServiceTemplate (Prelude.Maybe Prelude.Text)
serviceTemplate_description = Lens.lens (\ServiceTemplate' {description} -> description) (\s@ServiceTemplate' {} a -> s {description = a} :: ServiceTemplate) Prelude.. Lens.mapping Data._Sensitive

-- | The service template name as displayed in the developer interface.
serviceTemplate_displayName :: Lens.Lens' ServiceTemplate (Prelude.Maybe Prelude.Text)
serviceTemplate_displayName = Lens.lens (\ServiceTemplate' {displayName} -> displayName) (\s@ServiceTemplate' {} a -> s {displayName = a} :: ServiceTemplate) Prelude.. Lens.mapping Data._Sensitive

-- | The customer provided service template encryption key that\'s used to
-- encrypt data.
serviceTemplate_encryptionKey :: Lens.Lens' ServiceTemplate (Prelude.Maybe Prelude.Text)
serviceTemplate_encryptionKey = Lens.lens (\ServiceTemplate' {encryptionKey} -> encryptionKey) (\s@ServiceTemplate' {} a -> s {encryptionKey = a} :: ServiceTemplate)

-- | If @pipelineProvisioning@ is @true@, a service pipeline is included in
-- the service template. Otherwise, a service pipeline /isn\'t/ included in
-- the service template.
serviceTemplate_pipelineProvisioning :: Lens.Lens' ServiceTemplate (Prelude.Maybe Provisioning)
serviceTemplate_pipelineProvisioning = Lens.lens (\ServiceTemplate' {pipelineProvisioning} -> pipelineProvisioning) (\s@ServiceTemplate' {} a -> s {pipelineProvisioning = a} :: ServiceTemplate)

-- | The recommended version of the service template.
serviceTemplate_recommendedVersion :: Lens.Lens' ServiceTemplate (Prelude.Maybe Prelude.Text)
serviceTemplate_recommendedVersion = Lens.lens (\ServiceTemplate' {recommendedVersion} -> recommendedVersion) (\s@ServiceTemplate' {} a -> s {recommendedVersion = a} :: ServiceTemplate)

-- | The Amazon Resource Name (ARN) of the service template.
serviceTemplate_arn :: Lens.Lens' ServiceTemplate Prelude.Text
serviceTemplate_arn = Lens.lens (\ServiceTemplate' {arn} -> arn) (\s@ServiceTemplate' {} a -> s {arn = a} :: ServiceTemplate)

-- | The time when the service template was created.
serviceTemplate_createdAt :: Lens.Lens' ServiceTemplate Prelude.UTCTime
serviceTemplate_createdAt = Lens.lens (\ServiceTemplate' {createdAt} -> createdAt) (\s@ServiceTemplate' {} a -> s {createdAt = a} :: ServiceTemplate) Prelude.. Data._Time

-- | The time when the service template was last modified.
serviceTemplate_lastModifiedAt :: Lens.Lens' ServiceTemplate Prelude.UTCTime
serviceTemplate_lastModifiedAt = Lens.lens (\ServiceTemplate' {lastModifiedAt} -> lastModifiedAt) (\s@ServiceTemplate' {} a -> s {lastModifiedAt = a} :: ServiceTemplate) Prelude.. Data._Time

-- | The name of the service template.
serviceTemplate_name :: Lens.Lens' ServiceTemplate Prelude.Text
serviceTemplate_name = Lens.lens (\ServiceTemplate' {name} -> name) (\s@ServiceTemplate' {} a -> s {name = a} :: ServiceTemplate)

instance Data.FromJSON ServiceTemplate where
  parseJSON =
    Data.withObject
      "ServiceTemplate"
      ( \x ->
          ServiceTemplate'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "displayName")
            Prelude.<*> (x Data..:? "encryptionKey")
            Prelude.<*> (x Data..:? "pipelineProvisioning")
            Prelude.<*> (x Data..:? "recommendedVersion")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastModifiedAt")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable ServiceTemplate where
  hashWithSalt _salt ServiceTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` pipelineProvisioning
      `Prelude.hashWithSalt` recommendedVersion
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastModifiedAt
      `Prelude.hashWithSalt` name

instance Prelude.NFData ServiceTemplate where
  rnf ServiceTemplate' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf pipelineProvisioning
      `Prelude.seq` Prelude.rnf recommendedVersion
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastModifiedAt
      `Prelude.seq` Prelude.rnf name
