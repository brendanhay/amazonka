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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.Service where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.ServicePipeline
import Amazonka.Proton.Types.ServiceStatus

-- | Detailed data of an Proton service resource.
--
-- /See:/ 'newService' smart constructor.
data Service = Service'
  { -- | The name of the code repository branch that holds the code that\'s
    -- deployed in Proton.
    branchName :: Prelude.Maybe Prelude.Text,
    -- | A description of the service.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The service pipeline detail data.
    pipeline :: Prelude.Maybe ServicePipeline,
    -- | The Amazon Resource Name (ARN) of the repository connection. For more
    -- information, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/setting-up-for-service.html#setting-up-vcontrol Setting up an AWS CodeStar connection>
    -- in the /Proton User Guide/.
    repositoryConnectionArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the source code repository.
    repositoryId :: Prelude.Maybe Prelude.Text,
    -- | A service status message.
    statusMessage :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the service.
    arn :: Prelude.Text,
    -- | The time when the service was created.
    createdAt :: Data.POSIX,
    -- | The time when the service was last modified.
    lastModifiedAt :: Data.POSIX,
    -- | The name of the service.
    name :: Prelude.Text,
    -- | The formatted specification that defines the service.
    spec :: Data.Sensitive Prelude.Text,
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
-- deployed in Proton.
--
-- 'description', 'service_description' - A description of the service.
--
-- 'pipeline', 'service_pipeline' - The service pipeline detail data.
--
-- 'repositoryConnectionArn', 'service_repositoryConnectionArn' - The Amazon Resource Name (ARN) of the repository connection. For more
-- information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/setting-up-for-service.html#setting-up-vcontrol Setting up an AWS CodeStar connection>
-- in the /Proton User Guide/.
--
-- 'repositoryId', 'service_repositoryId' - The ID of the source code repository.
--
-- 'statusMessage', 'service_statusMessage' - A service status message.
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
        description = Prelude.Nothing,
        pipeline = Prelude.Nothing,
        repositoryConnectionArn = Prelude.Nothing,
        repositoryId = Prelude.Nothing,
        statusMessage = Prelude.Nothing,
        arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        lastModifiedAt = Data._Time Lens.# pLastModifiedAt_,
        name = pName_,
        spec = Data._Sensitive Lens.# pSpec_,
        status = pStatus_,
        templateName = pTemplateName_
      }

-- | The name of the code repository branch that holds the code that\'s
-- deployed in Proton.
service_branchName :: Lens.Lens' Service (Prelude.Maybe Prelude.Text)
service_branchName = Lens.lens (\Service' {branchName} -> branchName) (\s@Service' {} a -> s {branchName = a} :: Service)

-- | A description of the service.
service_description :: Lens.Lens' Service (Prelude.Maybe Prelude.Text)
service_description = Lens.lens (\Service' {description} -> description) (\s@Service' {} a -> s {description = a} :: Service) Prelude.. Lens.mapping Data._Sensitive

-- | The service pipeline detail data.
service_pipeline :: Lens.Lens' Service (Prelude.Maybe ServicePipeline)
service_pipeline = Lens.lens (\Service' {pipeline} -> pipeline) (\s@Service' {} a -> s {pipeline = a} :: Service)

-- | The Amazon Resource Name (ARN) of the repository connection. For more
-- information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/setting-up-for-service.html#setting-up-vcontrol Setting up an AWS CodeStar connection>
-- in the /Proton User Guide/.
service_repositoryConnectionArn :: Lens.Lens' Service (Prelude.Maybe Prelude.Text)
service_repositoryConnectionArn = Lens.lens (\Service' {repositoryConnectionArn} -> repositoryConnectionArn) (\s@Service' {} a -> s {repositoryConnectionArn = a} :: Service)

-- | The ID of the source code repository.
service_repositoryId :: Lens.Lens' Service (Prelude.Maybe Prelude.Text)
service_repositoryId = Lens.lens (\Service' {repositoryId} -> repositoryId) (\s@Service' {} a -> s {repositoryId = a} :: Service)

-- | A service status message.
service_statusMessage :: Lens.Lens' Service (Prelude.Maybe Prelude.Text)
service_statusMessage = Lens.lens (\Service' {statusMessage} -> statusMessage) (\s@Service' {} a -> s {statusMessage = a} :: Service) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) of the service.
service_arn :: Lens.Lens' Service Prelude.Text
service_arn = Lens.lens (\Service' {arn} -> arn) (\s@Service' {} a -> s {arn = a} :: Service)

-- | The time when the service was created.
service_createdAt :: Lens.Lens' Service Prelude.UTCTime
service_createdAt = Lens.lens (\Service' {createdAt} -> createdAt) (\s@Service' {} a -> s {createdAt = a} :: Service) Prelude.. Data._Time

-- | The time when the service was last modified.
service_lastModifiedAt :: Lens.Lens' Service Prelude.UTCTime
service_lastModifiedAt = Lens.lens (\Service' {lastModifiedAt} -> lastModifiedAt) (\s@Service' {} a -> s {lastModifiedAt = a} :: Service) Prelude.. Data._Time

-- | The name of the service.
service_name :: Lens.Lens' Service Prelude.Text
service_name = Lens.lens (\Service' {name} -> name) (\s@Service' {} a -> s {name = a} :: Service)

-- | The formatted specification that defines the service.
service_spec :: Lens.Lens' Service Prelude.Text
service_spec = Lens.lens (\Service' {spec} -> spec) (\s@Service' {} a -> s {spec = a} :: Service) Prelude.. Data._Sensitive

-- | The status of the service.
service_status :: Lens.Lens' Service ServiceStatus
service_status = Lens.lens (\Service' {status} -> status) (\s@Service' {} a -> s {status = a} :: Service)

-- | The name of the service template.
service_templateName :: Lens.Lens' Service Prelude.Text
service_templateName = Lens.lens (\Service' {templateName} -> templateName) (\s@Service' {} a -> s {templateName = a} :: Service)

instance Data.FromJSON Service where
  parseJSON =
    Data.withObject
      "Service"
      ( \x ->
          Service'
            Prelude.<$> (x Data..:? "branchName")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "pipeline")
            Prelude.<*> (x Data..:? "repositoryConnectionArn")
            Prelude.<*> (x Data..:? "repositoryId")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastModifiedAt")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "spec")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "templateName")
      )

instance Prelude.Hashable Service where
  hashWithSalt _salt Service' {..} =
    _salt
      `Prelude.hashWithSalt` branchName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` pipeline
      `Prelude.hashWithSalt` repositoryConnectionArn
      `Prelude.hashWithSalt` repositoryId
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastModifiedAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData Service where
  rnf Service' {..} =
    Prelude.rnf branchName `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf pipeline `Prelude.seq`
          Prelude.rnf repositoryConnectionArn `Prelude.seq`
            Prelude.rnf repositoryId `Prelude.seq`
              Prelude.rnf statusMessage `Prelude.seq`
                Prelude.rnf arn `Prelude.seq`
                  Prelude.rnf createdAt `Prelude.seq`
                    Prelude.rnf lastModifiedAt `Prelude.seq`
                      Prelude.rnf name `Prelude.seq`
                        Prelude.rnf spec `Prelude.seq`
                          Prelude.rnf status `Prelude.seq`
                            Prelude.rnf templateName
