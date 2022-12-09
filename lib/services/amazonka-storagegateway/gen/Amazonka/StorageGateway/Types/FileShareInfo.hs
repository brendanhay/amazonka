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
-- Module      : Amazonka.StorageGateway.Types.FileShareInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.FileShareInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.StorageGateway.Types.FileShareType

-- | Describes a file share. Only supported S3 File Gateway.
--
-- /See:/ 'newFileShareInfo' smart constructor.
data FileShareInfo = FileShareInfo'
  { fileShareARN :: Prelude.Maybe Prelude.Text,
    fileShareId :: Prelude.Maybe Prelude.Text,
    fileShareStatus :: Prelude.Maybe Prelude.Text,
    fileShareType :: Prelude.Maybe FileShareType,
    gatewayARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileShareInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileShareARN', 'fileShareInfo_fileShareARN' - Undocumented member.
--
-- 'fileShareId', 'fileShareInfo_fileShareId' - Undocumented member.
--
-- 'fileShareStatus', 'fileShareInfo_fileShareStatus' - Undocumented member.
--
-- 'fileShareType', 'fileShareInfo_fileShareType' - Undocumented member.
--
-- 'gatewayARN', 'fileShareInfo_gatewayARN' - Undocumented member.
newFileShareInfo ::
  FileShareInfo
newFileShareInfo =
  FileShareInfo'
    { fileShareARN = Prelude.Nothing,
      fileShareId = Prelude.Nothing,
      fileShareStatus = Prelude.Nothing,
      fileShareType = Prelude.Nothing,
      gatewayARN = Prelude.Nothing
    }

-- | Undocumented member.
fileShareInfo_fileShareARN :: Lens.Lens' FileShareInfo (Prelude.Maybe Prelude.Text)
fileShareInfo_fileShareARN = Lens.lens (\FileShareInfo' {fileShareARN} -> fileShareARN) (\s@FileShareInfo' {} a -> s {fileShareARN = a} :: FileShareInfo)

-- | Undocumented member.
fileShareInfo_fileShareId :: Lens.Lens' FileShareInfo (Prelude.Maybe Prelude.Text)
fileShareInfo_fileShareId = Lens.lens (\FileShareInfo' {fileShareId} -> fileShareId) (\s@FileShareInfo' {} a -> s {fileShareId = a} :: FileShareInfo)

-- | Undocumented member.
fileShareInfo_fileShareStatus :: Lens.Lens' FileShareInfo (Prelude.Maybe Prelude.Text)
fileShareInfo_fileShareStatus = Lens.lens (\FileShareInfo' {fileShareStatus} -> fileShareStatus) (\s@FileShareInfo' {} a -> s {fileShareStatus = a} :: FileShareInfo)

-- | Undocumented member.
fileShareInfo_fileShareType :: Lens.Lens' FileShareInfo (Prelude.Maybe FileShareType)
fileShareInfo_fileShareType = Lens.lens (\FileShareInfo' {fileShareType} -> fileShareType) (\s@FileShareInfo' {} a -> s {fileShareType = a} :: FileShareInfo)

-- | Undocumented member.
fileShareInfo_gatewayARN :: Lens.Lens' FileShareInfo (Prelude.Maybe Prelude.Text)
fileShareInfo_gatewayARN = Lens.lens (\FileShareInfo' {gatewayARN} -> gatewayARN) (\s@FileShareInfo' {} a -> s {gatewayARN = a} :: FileShareInfo)

instance Data.FromJSON FileShareInfo where
  parseJSON =
    Data.withObject
      "FileShareInfo"
      ( \x ->
          FileShareInfo'
            Prelude.<$> (x Data..:? "FileShareARN")
            Prelude.<*> (x Data..:? "FileShareId")
            Prelude.<*> (x Data..:? "FileShareStatus")
            Prelude.<*> (x Data..:? "FileShareType")
            Prelude.<*> (x Data..:? "GatewayARN")
      )

instance Prelude.Hashable FileShareInfo where
  hashWithSalt _salt FileShareInfo' {..} =
    _salt `Prelude.hashWithSalt` fileShareARN
      `Prelude.hashWithSalt` fileShareId
      `Prelude.hashWithSalt` fileShareStatus
      `Prelude.hashWithSalt` fileShareType
      `Prelude.hashWithSalt` gatewayARN

instance Prelude.NFData FileShareInfo where
  rnf FileShareInfo' {..} =
    Prelude.rnf fileShareARN
      `Prelude.seq` Prelude.rnf fileShareId
      `Prelude.seq` Prelude.rnf fileShareStatus
      `Prelude.seq` Prelude.rnf fileShareType
      `Prelude.seq` Prelude.rnf gatewayARN
