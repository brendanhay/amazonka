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
-- Module      : Amazonka.Proton.Types.Service
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.Service where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.ServicePipeline
import Amazonka.Proton.Types.ServiceStatus

-- | The service detail data.
--
-- /See:/ 'newService' smart constructor.
data Service = Service'
  { -- | The name of the code repository branch that holds the code that\'s
    -- deployed in AWS Proton.
    branchName :: Prelude.Maybe Prelude.Text,
    -- | A service status message.
    statusMessage :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of the code repository.
    repositoryId :: Prelude.Maybe Prelude.Text,
    -- | The service pipeline detail data.
    pipeline :: Prelude.Maybe ServicePipeline,
    -- | A description of a service.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the repository connection. For more
    -- information, see
    -- <https://docs.aws.amazon.com/proton/latest/adminguide/setting-up-for-service.html#setting-up-vcontrol Set up a repository connection>
    -- in the /AWS Proton Administrator Guide/ and
    -- <https://docs.aws.amazon.com/proton/latest/userguide/proton-setup.html#setup-repo-connection Setting up with AWS Proton>
    -- in the /AWS Proton User Guide/.
    repositoryConnectionArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the service.
    arn :: Prelude.Text,
    -- | The time when the service was created.
    createdAt :: Core.POSIX,
    -- | The time when the service was last modified.
    lastModifiedAt :: Core.POSIX,
    -- | The name of the service.
    name :: Prelude.Text,
    -- | The formatted specification that defines the service.
    spec :: Core.Sensitive Prelude.Text,
    -- | The status of the service.
    status :: ServiceStatus,
    -- | The name of the service template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Service' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branchName', 'service_branchName' - The name of the code repository branch that holds the code that\'s
-- deployed in AWS Proton.
--
-- 'statusMessage', 'service_statusMessage' - A service status message.
--
-- 'repositoryId', 'service_repositoryId' - The ID of the code repository.
--
-- 'pipeline', 'service_pipeline' - The service pipeline detail data.
--
-- 'description', 'service_description' - A description of a service.
--
-- 'repositoryConnectionArn', 'service_repositoryConnectionArn' - The Amazon Resource Name (ARN) of the repository connection. For more
-- information, see
-- <https://docs.aws.amazon.com/proton/latest/adminguide/setting-up-for-service.html#setting-up-vcontrol Set up a repository connection>
-- in the /AWS Proton Administrator Guide/ and
-- <https://docs.aws.amazon.com/proton/latest/userguide/proton-setup.html#setup-repo-connection Setting up with AWS Proton>
-- in the /AWS Proton User Guide/.
--
-- 'arn', 'service_arn' - The Amazon Resource Name (ARN) of the service.
--
-- 'createdAt', 'service_createdAt' - The time when the service was created.
--
-- 'lastModifiedAt', 'service_lastModifiedAt' - The time when the service was last modified.
--
-- 'name', 'service_name' - The name of the service.
--
-- 'spec', 'service_spec' - The formatted specification that defines the service.
--
-- 'status', 'service_status' - The status of the service.
--
-- 'templateName', 'service_templateName' - The name of the service template.
newService ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastModifiedAt'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  -- | 'spec'
  Prelude.Text ->
  -- | 'status'
  ServiceStatus ->
  -- | 'templateName'
  Prelude.Text ->
  Service
newService
  pArn_
  pCreatedAt_
  pLastModifiedAt_
  pName_
  pSpec_
  pStatus_
  pTemplateName_ =
    Service'
      { branchName = Prelude.Nothing,
        statusMessage = Prelude.Nothing,
        repositoryId = Prelude.Nothing,
        pipeline = Prelude.Nothing,
        description = Prelude.Nothing,
        repositoryConnectionArn = Prelude.Nothing,
        arn = pArn_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        lastModifiedAt = Core._Time Lens.# pLastModifiedAt_,
        name = pName_,
        spec = Core._Sensitive Lens.# pSpec_,
        status = pStatus_,
        templateName = pTemplateName_
      }

-- | The name of the code repository branch that holds the code that\'s
-- deployed in AWS Proton.
service_branchName :: Lens.Lens' Service (Prelude.Maybe Prelude.Text)
service_branchName = Lens.lens (\Service' {branchName} -> branchName) (\s@Service' {} a -> s {branchName = a} :: Service)

-- | A service status message.
service_statusMessage :: Lens.Lens' Service (Prelude.Maybe Prelude.Text)
service_statusMessage = Lens.lens (\Service' {statusMessage} -> statusMessage) (\s@Service' {} a -> s {statusMessage = a} :: Service) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of the code repository.
service_repositoryId :: Lens.Lens' Service (Prelude.Maybe Prelude.Text)
service_repositoryId = Lens.lens (\Service' {repositoryId} -> repositoryId) (\s@Service' {} a -> s {repositoryId = a} :: Service)

-- | The service pipeline detail data.
service_pipeline :: Lens.Lens' Service (Prelude.Maybe ServicePipeline)
service_pipeline = Lens.lens (\Service' {pipeline} -> pipeline) (\s@Service' {} a -> s {pipeline = a} :: Service)

-- | A description of a service.
service_description :: Lens.Lens' Service (Prelude.Maybe Prelude.Text)
service_description = Lens.lens (\Service' {description} -> description) (\s@Service' {} a -> s {description = a} :: Service) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) of the repository connection. For more
-- information, see
-- <https://docs.aws.amazon.com/proton/latest/adminguide/setting-up-for-service.html#setting-up-vcontrol Set up a repository connection>
-- in the /AWS Proton Administrator Guide/ and
-- <https://docs.aws.amazon.com/proton/latest/userguide/proton-setup.html#setup-repo-connection Setting up with AWS Proton>
-- in the /AWS Proton User Guide/.
service_repositoryConnectionArn :: Lens.Lens' Service (Prelude.Maybe Prelude.Text)
service_repositoryConnectionArn = Lens.lens (\Service' {repositoryConnectionArn} -> repositoryConnectionArn) (\s@Service' {} a -> s {repositoryConnectionArn = a} :: Service)

-- | The Amazon Resource Name (ARN) of the service.
service_arn :: Lens.Lens' Service Prelude.Text
service_arn = Lens.lens (\Service' {arn} -> arn) (\s@Service' {} a -> s {arn = a} :: Service)

-- | The time when the service was created.
service_createdAt :: Lens.Lens' Service Prelude.UTCTime
service_createdAt = Lens.lens (\Service' {createdAt} -> createdAt) (\s@Service' {} a -> s {createdAt = a} :: Service) Prelude.. Core._Time

-- | The time when the service was last modified.
service_lastModifiedAt :: Lens.Lens' Service Prelude.UTCTime
service_lastModifiedAt = Lens.lens (\Service' {lastModifiedAt} -> lastModifiedAt) (\s@Service' {} a -> s {lastModifiedAt = a} :: Service) Prelude.. Core._Time

-- | The name of the service.
service_name :: Lens.Lens' Service Prelude.Text
service_name = Lens.lens (\Service' {name} -> name) (\s@Service' {} a -> s {name = a} :: Service)

-- | The formatted specification that defines the service.
service_spec :: Lens.Lens' Service Prelude.Text
service_spec = Lens.lens (\Service' {spec} -> spec) (\s@Service' {} a -> s {spec = a} :: Service) Prelude.. Core._Sensitive

-- | The status of the service.
service_status :: Lens.Lens' Service ServiceStatus
service_status = Lens.lens (\Service' {status} -> status) (\s@Service' {} a -> s {status = a} :: Service)

-- | The name of the service template.
service_templateName :: Lens.Lens' Service Prelude.Text
service_templateName = Lens.lens (\Service' {templateName} -> templateName) (\s@Service' {} a -> s {templateName = a} :: Service)

instance Core.FromJSON Service where
  parseJSON =
    Core.withObject
      "Service"
      ( \x ->
          Service'
            Prelude.<$> (x Core..:? "branchName")
            Prelude.<*> (x Core..:? "statusMessage")
            Prelude.<*> (x Core..:? "repositoryId")
            Prelude.<*> (x Core..:? "pipeline")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "repositoryConnectionArn")
            Prelude.<*> (x Core..: "arn")
            Prelude.<*> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "lastModifiedAt")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "spec")
            Prelude.<*> (x Core..: "status")
            Prelude.<*> (x Core..: "templateName")
      )

instance Prelude.Hashable Service where
  hashWithSalt _salt Service' {..} =
    _salt `Prelude.hashWithSalt` branchName
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` repositoryId
      `Prelude.hashWithSalt` pipeline
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` repositoryConnectionArn
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastModifiedAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData Service where
  rnf Service' {..} =
    Prelude.rnf branchName
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf repositoryId
      `Prelude.seq` Prelude.rnf pipeline
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf repositoryConnectionArn
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastModifiedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf templateName
