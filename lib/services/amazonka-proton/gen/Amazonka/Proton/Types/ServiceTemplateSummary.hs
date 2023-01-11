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
-- Module      : Amazonka.Proton.Types.ServiceTemplateSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ServiceTemplateSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.Provisioning

-- | Summary data of an Proton service template resource.
--
-- /See:/ 'newServiceTemplateSummary' smart constructor.
data ServiceTemplateSummary = ServiceTemplateSummary'
  { -- | A description of the service template.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The service template name as displayed in the developer interface.
    displayName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | If @pipelineProvisioning@ is @true@, a service pipeline is included in
    -- the service template, otherwise a service pipeline /isn\'t/ included in
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
-- Create a value of 'ServiceTemplateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'serviceTemplateSummary_description' - A description of the service template.
--
-- 'displayName', 'serviceTemplateSummary_displayName' - The service template name as displayed in the developer interface.
--
-- 'pipelineProvisioning', 'serviceTemplateSummary_pipelineProvisioning' - If @pipelineProvisioning@ is @true@, a service pipeline is included in
-- the service template, otherwise a service pipeline /isn\'t/ included in
-- the service template.
--
-- 'recommendedVersion', 'serviceTemplateSummary_recommendedVersion' - The recommended version of the service template.
--
-- 'arn', 'serviceTemplateSummary_arn' - The Amazon Resource Name (ARN) of the service template.
--
-- 'createdAt', 'serviceTemplateSummary_createdAt' - The time when the service template was created.
--
-- 'lastModifiedAt', 'serviceTemplateSummary_lastModifiedAt' - The time when the service template was last modified.
--
-- 'name', 'serviceTemplateSummary_name' - The name of the service template.
newServiceTemplateSummary ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastModifiedAt'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  ServiceTemplateSummary
newServiceTemplateSummary
  pArn_
  pCreatedAt_
  pLastModifiedAt_
  pName_ =
    ServiceTemplateSummary'
      { description =
          Prelude.Nothing,
        displayName = Prelude.Nothing,
        pipelineProvisioning = Prelude.Nothing,
        recommendedVersion = Prelude.Nothing,
        arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        lastModifiedAt = Data._Time Lens.# pLastModifiedAt_,
        name = pName_
      }

-- | A description of the service template.
serviceTemplateSummary_description :: Lens.Lens' ServiceTemplateSummary (Prelude.Maybe Prelude.Text)
serviceTemplateSummary_description = Lens.lens (\ServiceTemplateSummary' {description} -> description) (\s@ServiceTemplateSummary' {} a -> s {description = a} :: ServiceTemplateSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The service template name as displayed in the developer interface.
serviceTemplateSummary_displayName :: Lens.Lens' ServiceTemplateSummary (Prelude.Maybe Prelude.Text)
serviceTemplateSummary_displayName = Lens.lens (\ServiceTemplateSummary' {displayName} -> displayName) (\s@ServiceTemplateSummary' {} a -> s {displayName = a} :: ServiceTemplateSummary) Prelude.. Lens.mapping Data._Sensitive

-- | If @pipelineProvisioning@ is @true@, a service pipeline is included in
-- the service template, otherwise a service pipeline /isn\'t/ included in
-- the service template.
serviceTemplateSummary_pipelineProvisioning :: Lens.Lens' ServiceTemplateSummary (Prelude.Maybe Provisioning)
serviceTemplateSummary_pipelineProvisioning = Lens.lens (\ServiceTemplateSummary' {pipelineProvisioning} -> pipelineProvisioning) (\s@ServiceTemplateSummary' {} a -> s {pipelineProvisioning = a} :: ServiceTemplateSummary)

-- | The recommended version of the service template.
serviceTemplateSummary_recommendedVersion :: Lens.Lens' ServiceTemplateSummary (Prelude.Maybe Prelude.Text)
serviceTemplateSummary_recommendedVersion = Lens.lens (\ServiceTemplateSummary' {recommendedVersion} -> recommendedVersion) (\s@ServiceTemplateSummary' {} a -> s {recommendedVersion = a} :: ServiceTemplateSummary)

-- | The Amazon Resource Name (ARN) of the service template.
serviceTemplateSummary_arn :: Lens.Lens' ServiceTemplateSummary Prelude.Text
serviceTemplateSummary_arn = Lens.lens (\ServiceTemplateSummary' {arn} -> arn) (\s@ServiceTemplateSummary' {} a -> s {arn = a} :: ServiceTemplateSummary)

-- | The time when the service template was created.
serviceTemplateSummary_createdAt :: Lens.Lens' ServiceTemplateSummary Prelude.UTCTime
serviceTemplateSummary_createdAt = Lens.lens (\ServiceTemplateSummary' {createdAt} -> createdAt) (\s@ServiceTemplateSummary' {} a -> s {createdAt = a} :: ServiceTemplateSummary) Prelude.. Data._Time

-- | The time when the service template was last modified.
serviceTemplateSummary_lastModifiedAt :: Lens.Lens' ServiceTemplateSummary Prelude.UTCTime
serviceTemplateSummary_lastModifiedAt = Lens.lens (\ServiceTemplateSummary' {lastModifiedAt} -> lastModifiedAt) (\s@ServiceTemplateSummary' {} a -> s {lastModifiedAt = a} :: ServiceTemplateSummary) Prelude.. Data._Time

-- | The name of the service template.
serviceTemplateSummary_name :: Lens.Lens' ServiceTemplateSummary Prelude.Text
serviceTemplateSummary_name = Lens.lens (\ServiceTemplateSummary' {name} -> name) (\s@ServiceTemplateSummary' {} a -> s {name = a} :: ServiceTemplateSummary)

instance Data.FromJSON ServiceTemplateSummary where
  parseJSON =
    Data.withObject
      "ServiceTemplateSummary"
      ( \x ->
          ServiceTemplateSummary'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "displayName")
            Prelude.<*> (x Data..:? "pipelineProvisioning")
            Prelude.<*> (x Data..:? "recommendedVersion")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastModifiedAt")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable ServiceTemplateSummary where
  hashWithSalt _salt ServiceTemplateSummary' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` pipelineProvisioning
      `Prelude.hashWithSalt` recommendedVersion
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastModifiedAt
      `Prelude.hashWithSalt` name

instance Prelude.NFData ServiceTemplateSummary where
  rnf ServiceTemplateSummary' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf pipelineProvisioning
      `Prelude.seq` Prelude.rnf recommendedVersion
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastModifiedAt
      `Prelude.seq` Prelude.rnf name
