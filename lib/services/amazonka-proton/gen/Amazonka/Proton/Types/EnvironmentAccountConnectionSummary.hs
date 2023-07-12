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
-- Module      : Amazonka.Proton.Types.EnvironmentAccountConnectionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.EnvironmentAccountConnectionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.EnvironmentAccountConnectionStatus

-- | Summary data of an Proton environment account connection resource.
--
-- /See:/ 'newEnvironmentAccountConnectionSummary' smart constructor.
data EnvironmentAccountConnectionSummary = EnvironmentAccountConnectionSummary'
  { -- | The Amazon Resource Name (ARN) of the IAM service role that Proton uses
    -- when provisioning directly defined components in the associated
    -- environment account. It determines the scope of infrastructure that a
    -- component can provision in the account.
    --
    -- The environment account connection must have a @componentRoleArn@ to
    -- allow directly defined components to be associated with any environments
    -- running in the account.
    --
    -- For more information about components, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
    -- in the /Proton User Guide/.
    componentRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the environment account connection.
    arn :: Prelude.Text,
    -- | The ID of the environment account that\'s connected to the environment
    -- account connection.
    environmentAccountId :: Prelude.Text,
    -- | The name of the environment that\'s associated with the environment
    -- account connection.
    environmentName :: Prelude.Text,
    -- | The ID of the environment account connection.
    id :: Prelude.Text,
    -- | The time when the environment account connection was last modified.
    lastModifiedAt :: Data.POSIX,
    -- | The ID of the management account that\'s connected to the environment
    -- account connection.
    managementAccountId :: Prelude.Text,
    -- | The time when the environment account connection request was made.
    requestedAt :: Data.POSIX,
    -- | The IAM service role that\'s associated with the environment account
    -- connection.
    roleArn :: Prelude.Text,
    -- | The status of the environment account connection.
    status :: EnvironmentAccountConnectionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentAccountConnectionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentRoleArn', 'environmentAccountConnectionSummary_componentRoleArn' - The Amazon Resource Name (ARN) of the IAM service role that Proton uses
-- when provisioning directly defined components in the associated
-- environment account. It determines the scope of infrastructure that a
-- component can provision in the account.
--
-- The environment account connection must have a @componentRoleArn@ to
-- allow directly defined components to be associated with any environments
-- running in the account.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
--
-- 'arn', 'environmentAccountConnectionSummary_arn' - The Amazon Resource Name (ARN) of the environment account connection.
--
-- 'environmentAccountId', 'environmentAccountConnectionSummary_environmentAccountId' - The ID of the environment account that\'s connected to the environment
-- account connection.
--
-- 'environmentName', 'environmentAccountConnectionSummary_environmentName' - The name of the environment that\'s associated with the environment
-- account connection.
--
-- 'id', 'environmentAccountConnectionSummary_id' - The ID of the environment account connection.
--
-- 'lastModifiedAt', 'environmentAccountConnectionSummary_lastModifiedAt' - The time when the environment account connection was last modified.
--
-- 'managementAccountId', 'environmentAccountConnectionSummary_managementAccountId' - The ID of the management account that\'s connected to the environment
-- account connection.
--
-- 'requestedAt', 'environmentAccountConnectionSummary_requestedAt' - The time when the environment account connection request was made.
--
-- 'roleArn', 'environmentAccountConnectionSummary_roleArn' - The IAM service role that\'s associated with the environment account
-- connection.
--
-- 'status', 'environmentAccountConnectionSummary_status' - The status of the environment account connection.
newEnvironmentAccountConnectionSummary ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'environmentAccountId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'lastModifiedAt'
  Prelude.UTCTime ->
  -- | 'managementAccountId'
  Prelude.Text ->
  -- | 'requestedAt'
  Prelude.UTCTime ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'status'
  EnvironmentAccountConnectionStatus ->
  EnvironmentAccountConnectionSummary
newEnvironmentAccountConnectionSummary
  pArn_
  pEnvironmentAccountId_
  pEnvironmentName_
  pId_
  pLastModifiedAt_
  pManagementAccountId_
  pRequestedAt_
  pRoleArn_
  pStatus_ =
    EnvironmentAccountConnectionSummary'
      { componentRoleArn =
          Prelude.Nothing,
        arn = pArn_,
        environmentAccountId =
          pEnvironmentAccountId_,
        environmentName = pEnvironmentName_,
        id = pId_,
        lastModifiedAt =
          Data._Time Lens.# pLastModifiedAt_,
        managementAccountId =
          pManagementAccountId_,
        requestedAt =
          Data._Time Lens.# pRequestedAt_,
        roleArn = pRoleArn_,
        status = pStatus_
      }

-- | The Amazon Resource Name (ARN) of the IAM service role that Proton uses
-- when provisioning directly defined components in the associated
-- environment account. It determines the scope of infrastructure that a
-- component can provision in the account.
--
-- The environment account connection must have a @componentRoleArn@ to
-- allow directly defined components to be associated with any environments
-- running in the account.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
environmentAccountConnectionSummary_componentRoleArn :: Lens.Lens' EnvironmentAccountConnectionSummary (Prelude.Maybe Prelude.Text)
environmentAccountConnectionSummary_componentRoleArn = Lens.lens (\EnvironmentAccountConnectionSummary' {componentRoleArn} -> componentRoleArn) (\s@EnvironmentAccountConnectionSummary' {} a -> s {componentRoleArn = a} :: EnvironmentAccountConnectionSummary)

-- | The Amazon Resource Name (ARN) of the environment account connection.
environmentAccountConnectionSummary_arn :: Lens.Lens' EnvironmentAccountConnectionSummary Prelude.Text
environmentAccountConnectionSummary_arn = Lens.lens (\EnvironmentAccountConnectionSummary' {arn} -> arn) (\s@EnvironmentAccountConnectionSummary' {} a -> s {arn = a} :: EnvironmentAccountConnectionSummary)

-- | The ID of the environment account that\'s connected to the environment
-- account connection.
environmentAccountConnectionSummary_environmentAccountId :: Lens.Lens' EnvironmentAccountConnectionSummary Prelude.Text
environmentAccountConnectionSummary_environmentAccountId = Lens.lens (\EnvironmentAccountConnectionSummary' {environmentAccountId} -> environmentAccountId) (\s@EnvironmentAccountConnectionSummary' {} a -> s {environmentAccountId = a} :: EnvironmentAccountConnectionSummary)

-- | The name of the environment that\'s associated with the environment
-- account connection.
environmentAccountConnectionSummary_environmentName :: Lens.Lens' EnvironmentAccountConnectionSummary Prelude.Text
environmentAccountConnectionSummary_environmentName = Lens.lens (\EnvironmentAccountConnectionSummary' {environmentName} -> environmentName) (\s@EnvironmentAccountConnectionSummary' {} a -> s {environmentName = a} :: EnvironmentAccountConnectionSummary)

-- | The ID of the environment account connection.
environmentAccountConnectionSummary_id :: Lens.Lens' EnvironmentAccountConnectionSummary Prelude.Text
environmentAccountConnectionSummary_id = Lens.lens (\EnvironmentAccountConnectionSummary' {id} -> id) (\s@EnvironmentAccountConnectionSummary' {} a -> s {id = a} :: EnvironmentAccountConnectionSummary)

-- | The time when the environment account connection was last modified.
environmentAccountConnectionSummary_lastModifiedAt :: Lens.Lens' EnvironmentAccountConnectionSummary Prelude.UTCTime
environmentAccountConnectionSummary_lastModifiedAt = Lens.lens (\EnvironmentAccountConnectionSummary' {lastModifiedAt} -> lastModifiedAt) (\s@EnvironmentAccountConnectionSummary' {} a -> s {lastModifiedAt = a} :: EnvironmentAccountConnectionSummary) Prelude.. Data._Time

-- | The ID of the management account that\'s connected to the environment
-- account connection.
environmentAccountConnectionSummary_managementAccountId :: Lens.Lens' EnvironmentAccountConnectionSummary Prelude.Text
environmentAccountConnectionSummary_managementAccountId = Lens.lens (\EnvironmentAccountConnectionSummary' {managementAccountId} -> managementAccountId) (\s@EnvironmentAccountConnectionSummary' {} a -> s {managementAccountId = a} :: EnvironmentAccountConnectionSummary)

-- | The time when the environment account connection request was made.
environmentAccountConnectionSummary_requestedAt :: Lens.Lens' EnvironmentAccountConnectionSummary Prelude.UTCTime
environmentAccountConnectionSummary_requestedAt = Lens.lens (\EnvironmentAccountConnectionSummary' {requestedAt} -> requestedAt) (\s@EnvironmentAccountConnectionSummary' {} a -> s {requestedAt = a} :: EnvironmentAccountConnectionSummary) Prelude.. Data._Time

-- | The IAM service role that\'s associated with the environment account
-- connection.
environmentAccountConnectionSummary_roleArn :: Lens.Lens' EnvironmentAccountConnectionSummary Prelude.Text
environmentAccountConnectionSummary_roleArn = Lens.lens (\EnvironmentAccountConnectionSummary' {roleArn} -> roleArn) (\s@EnvironmentAccountConnectionSummary' {} a -> s {roleArn = a} :: EnvironmentAccountConnectionSummary)

-- | The status of the environment account connection.
environmentAccountConnectionSummary_status :: Lens.Lens' EnvironmentAccountConnectionSummary EnvironmentAccountConnectionStatus
environmentAccountConnectionSummary_status = Lens.lens (\EnvironmentAccountConnectionSummary' {status} -> status) (\s@EnvironmentAccountConnectionSummary' {} a -> s {status = a} :: EnvironmentAccountConnectionSummary)

instance
  Data.FromJSON
    EnvironmentAccountConnectionSummary
  where
  parseJSON =
    Data.withObject
      "EnvironmentAccountConnectionSummary"
      ( \x ->
          EnvironmentAccountConnectionSummary'
            Prelude.<$> (x Data..:? "componentRoleArn")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "environmentAccountId")
            Prelude.<*> (x Data..: "environmentName")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "lastModifiedAt")
            Prelude.<*> (x Data..: "managementAccountId")
            Prelude.<*> (x Data..: "requestedAt")
            Prelude.<*> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "status")
      )

instance
  Prelude.Hashable
    EnvironmentAccountConnectionSummary
  where
  hashWithSalt
    _salt
    EnvironmentAccountConnectionSummary' {..} =
      _salt
        `Prelude.hashWithSalt` componentRoleArn
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` environmentAccountId
        `Prelude.hashWithSalt` environmentName
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` lastModifiedAt
        `Prelude.hashWithSalt` managementAccountId
        `Prelude.hashWithSalt` requestedAt
        `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    EnvironmentAccountConnectionSummary
  where
  rnf EnvironmentAccountConnectionSummary' {..} =
    Prelude.rnf componentRoleArn
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf environmentAccountId
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastModifiedAt
      `Prelude.seq` Prelude.rnf managementAccountId
      `Prelude.seq` Prelude.rnf requestedAt
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf status
