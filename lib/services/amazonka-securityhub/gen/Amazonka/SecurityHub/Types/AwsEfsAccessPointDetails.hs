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
-- Module      : Amazonka.SecurityHub.Types.AwsEfsAccessPointDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEfsAccessPointDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEfsAccessPointPosixUserDetails
import Amazonka.SecurityHub.Types.AwsEfsAccessPointRootDirectoryDetails

-- | Provides information about an Amazon EFS access point.
--
-- /See:/ 'newAwsEfsAccessPointDetails' smart constructor.
data AwsEfsAccessPointDetails = AwsEfsAccessPointDetails'
  { -- | The ID of the Amazon EFS access point.
    accessPointId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon EFS access point.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The opaque string specified in the request to ensure idempotent
    -- creation.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon EFS file system that the access point applies to.
    fileSystemId :: Prelude.Maybe Prelude.Text,
    -- | The full POSIX identity, including the user ID, group ID, and secondary
    -- group IDs on the access point, that is used for all file operations by
    -- NFS clients using the access point.
    posixUser :: Prelude.Maybe AwsEfsAccessPointPosixUserDetails,
    -- | The directory on the Amazon EFS file system that the access point
    -- exposes as the root directory to NFS clients using the access point.
    rootDirectory :: Prelude.Maybe AwsEfsAccessPointRootDirectoryDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEfsAccessPointDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessPointId', 'awsEfsAccessPointDetails_accessPointId' - The ID of the Amazon EFS access point.
--
-- 'arn', 'awsEfsAccessPointDetails_arn' - The Amazon Resource Name (ARN) of the Amazon EFS access point.
--
-- 'clientToken', 'awsEfsAccessPointDetails_clientToken' - The opaque string specified in the request to ensure idempotent
-- creation.
--
-- 'fileSystemId', 'awsEfsAccessPointDetails_fileSystemId' - The ID of the Amazon EFS file system that the access point applies to.
--
-- 'posixUser', 'awsEfsAccessPointDetails_posixUser' - The full POSIX identity, including the user ID, group ID, and secondary
-- group IDs on the access point, that is used for all file operations by
-- NFS clients using the access point.
--
-- 'rootDirectory', 'awsEfsAccessPointDetails_rootDirectory' - The directory on the Amazon EFS file system that the access point
-- exposes as the root directory to NFS clients using the access point.
newAwsEfsAccessPointDetails ::
  AwsEfsAccessPointDetails
newAwsEfsAccessPointDetails =
  AwsEfsAccessPointDetails'
    { accessPointId =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      fileSystemId = Prelude.Nothing,
      posixUser = Prelude.Nothing,
      rootDirectory = Prelude.Nothing
    }

-- | The ID of the Amazon EFS access point.
awsEfsAccessPointDetails_accessPointId :: Lens.Lens' AwsEfsAccessPointDetails (Prelude.Maybe Prelude.Text)
awsEfsAccessPointDetails_accessPointId = Lens.lens (\AwsEfsAccessPointDetails' {accessPointId} -> accessPointId) (\s@AwsEfsAccessPointDetails' {} a -> s {accessPointId = a} :: AwsEfsAccessPointDetails)

-- | The Amazon Resource Name (ARN) of the Amazon EFS access point.
awsEfsAccessPointDetails_arn :: Lens.Lens' AwsEfsAccessPointDetails (Prelude.Maybe Prelude.Text)
awsEfsAccessPointDetails_arn = Lens.lens (\AwsEfsAccessPointDetails' {arn} -> arn) (\s@AwsEfsAccessPointDetails' {} a -> s {arn = a} :: AwsEfsAccessPointDetails)

-- | The opaque string specified in the request to ensure idempotent
-- creation.
awsEfsAccessPointDetails_clientToken :: Lens.Lens' AwsEfsAccessPointDetails (Prelude.Maybe Prelude.Text)
awsEfsAccessPointDetails_clientToken = Lens.lens (\AwsEfsAccessPointDetails' {clientToken} -> clientToken) (\s@AwsEfsAccessPointDetails' {} a -> s {clientToken = a} :: AwsEfsAccessPointDetails)

-- | The ID of the Amazon EFS file system that the access point applies to.
awsEfsAccessPointDetails_fileSystemId :: Lens.Lens' AwsEfsAccessPointDetails (Prelude.Maybe Prelude.Text)
awsEfsAccessPointDetails_fileSystemId = Lens.lens (\AwsEfsAccessPointDetails' {fileSystemId} -> fileSystemId) (\s@AwsEfsAccessPointDetails' {} a -> s {fileSystemId = a} :: AwsEfsAccessPointDetails)

-- | The full POSIX identity, including the user ID, group ID, and secondary
-- group IDs on the access point, that is used for all file operations by
-- NFS clients using the access point.
awsEfsAccessPointDetails_posixUser :: Lens.Lens' AwsEfsAccessPointDetails (Prelude.Maybe AwsEfsAccessPointPosixUserDetails)
awsEfsAccessPointDetails_posixUser = Lens.lens (\AwsEfsAccessPointDetails' {posixUser} -> posixUser) (\s@AwsEfsAccessPointDetails' {} a -> s {posixUser = a} :: AwsEfsAccessPointDetails)

-- | The directory on the Amazon EFS file system that the access point
-- exposes as the root directory to NFS clients using the access point.
awsEfsAccessPointDetails_rootDirectory :: Lens.Lens' AwsEfsAccessPointDetails (Prelude.Maybe AwsEfsAccessPointRootDirectoryDetails)
awsEfsAccessPointDetails_rootDirectory = Lens.lens (\AwsEfsAccessPointDetails' {rootDirectory} -> rootDirectory) (\s@AwsEfsAccessPointDetails' {} a -> s {rootDirectory = a} :: AwsEfsAccessPointDetails)

instance Data.FromJSON AwsEfsAccessPointDetails where
  parseJSON =
    Data.withObject
      "AwsEfsAccessPointDetails"
      ( \x ->
          AwsEfsAccessPointDetails'
            Prelude.<$> (x Data..:? "AccessPointId")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "ClientToken")
            Prelude.<*> (x Data..:? "FileSystemId")
            Prelude.<*> (x Data..:? "PosixUser")
            Prelude.<*> (x Data..:? "RootDirectory")
      )

instance Prelude.Hashable AwsEfsAccessPointDetails where
  hashWithSalt _salt AwsEfsAccessPointDetails' {..} =
    _salt
      `Prelude.hashWithSalt` accessPointId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` posixUser
      `Prelude.hashWithSalt` rootDirectory

instance Prelude.NFData AwsEfsAccessPointDetails where
  rnf AwsEfsAccessPointDetails' {..} =
    Prelude.rnf accessPointId `Prelude.seq`
      Prelude.rnf arn `Prelude.seq`
        Prelude.rnf clientToken `Prelude.seq`
          Prelude.rnf fileSystemId `Prelude.seq`
            Prelude.rnf posixUser `Prelude.seq`
              Prelude.rnf rootDirectory

instance Data.ToJSON AwsEfsAccessPointDetails where
  toJSON AwsEfsAccessPointDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessPointId" Data..=) Prelude.<$> accessPointId,
            ("Arn" Data..=) Prelude.<$> arn,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("FileSystemId" Data..=) Prelude.<$> fileSystemId,
            ("PosixUser" Data..=) Prelude.<$> posixUser,
            ("RootDirectory" Data..=) Prelude.<$> rootDirectory
          ]
      )
