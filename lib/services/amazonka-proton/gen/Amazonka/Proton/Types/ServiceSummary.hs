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
-- Module      : Amazonka.Proton.Types.ServiceSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ServiceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.ServiceStatus

-- | Summary data of an Proton service resource.
--
-- /See:/ 'newServiceSummary' smart constructor.
data ServiceSummary = ServiceSummary'
  { -- | A description of the service.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
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
    -- | The status of the service.
    status :: ServiceStatus,
    -- | The name of the service template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'serviceSummary_description' - A description of the service.
--
-- 'statusMessage', 'serviceSummary_statusMessage' - A service status message.
--
-- 'arn', 'serviceSummary_arn' - The Amazon Resource Name (ARN) of the service.
--
-- 'createdAt', 'serviceSummary_createdAt' - The time when the service was created.
--
-- 'lastModifiedAt', 'serviceSummary_lastModifiedAt' - The time when the service was last modified.
--
-- 'name', 'serviceSummary_name' - The name of the service.
--
-- 'status', 'serviceSummary_status' - The status of the service.
--
-- 'templateName', 'serviceSummary_templateName' - The name of the service template.
newServiceSummary ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastModifiedAt'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  ServiceStatus ->
  -- | 'templateName'
  Prelude.Text ->
  ServiceSummary
newServiceSummary
  pArn_
  pCreatedAt_
  pLastModifiedAt_
  pName_
  pStatus_
  pTemplateName_ =
    ServiceSummary'
      { description = Prelude.Nothing,
        statusMessage = Prelude.Nothing,
        arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        lastModifiedAt = Data._Time Lens.# pLastModifiedAt_,
        name = pName_,
        status = pStatus_,
        templateName = pTemplateName_
      }

-- | A description of the service.
serviceSummary_description :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_description = Lens.lens (\ServiceSummary' {description} -> description) (\s@ServiceSummary' {} a -> s {description = a} :: ServiceSummary) Prelude.. Lens.mapping Data._Sensitive

-- | A service status message.
serviceSummary_statusMessage :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_statusMessage = Lens.lens (\ServiceSummary' {statusMessage} -> statusMessage) (\s@ServiceSummary' {} a -> s {statusMessage = a} :: ServiceSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) of the service.
serviceSummary_arn :: Lens.Lens' ServiceSummary Prelude.Text
serviceSummary_arn = Lens.lens (\ServiceSummary' {arn} -> arn) (\s@ServiceSummary' {} a -> s {arn = a} :: ServiceSummary)

-- | The time when the service was created.
serviceSummary_createdAt :: Lens.Lens' ServiceSummary Prelude.UTCTime
serviceSummary_createdAt = Lens.lens (\ServiceSummary' {createdAt} -> createdAt) (\s@ServiceSummary' {} a -> s {createdAt = a} :: ServiceSummary) Prelude.. Data._Time

-- | The time when the service was last modified.
serviceSummary_lastModifiedAt :: Lens.Lens' ServiceSummary Prelude.UTCTime
serviceSummary_lastModifiedAt = Lens.lens (\ServiceSummary' {lastModifiedAt} -> lastModifiedAt) (\s@ServiceSummary' {} a -> s {lastModifiedAt = a} :: ServiceSummary) Prelude.. Data._Time

-- | The name of the service.
serviceSummary_name :: Lens.Lens' ServiceSummary Prelude.Text
serviceSummary_name = Lens.lens (\ServiceSummary' {name} -> name) (\s@ServiceSummary' {} a -> s {name = a} :: ServiceSummary)

-- | The status of the service.
serviceSummary_status :: Lens.Lens' ServiceSummary ServiceStatus
serviceSummary_status = Lens.lens (\ServiceSummary' {status} -> status) (\s@ServiceSummary' {} a -> s {status = a} :: ServiceSummary)

-- | The name of the service template.
serviceSummary_templateName :: Lens.Lens' ServiceSummary Prelude.Text
serviceSummary_templateName = Lens.lens (\ServiceSummary' {templateName} -> templateName) (\s@ServiceSummary' {} a -> s {templateName = a} :: ServiceSummary)

instance Data.FromJSON ServiceSummary where
  parseJSON =
    Data.withObject
      "ServiceSummary"
      ( \x ->
          ServiceSummary'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastModifiedAt")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "templateName")
      )

instance Prelude.Hashable ServiceSummary where
  hashWithSalt _salt ServiceSummary' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastModifiedAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData ServiceSummary where
  rnf ServiceSummary' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastModifiedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf templateName
