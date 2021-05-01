{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.StorageGateway.Types.FileShareInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.FileShareInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.StorageGateway.Types.FileShareType

-- | Describes a file share.
--
-- /See:/ 'newFileShareInfo' smart constructor.
data FileShareInfo = FileShareInfo'
  { fileShareId :: Prelude.Maybe Prelude.Text,
    fileShareType :: Prelude.Maybe FileShareType,
    fileShareStatus :: Prelude.Maybe Prelude.Text,
    fileShareARN :: Prelude.Maybe Prelude.Text,
    gatewayARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FileShareInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileShareId', 'fileShareInfo_fileShareId' - Undocumented member.
--
-- 'fileShareType', 'fileShareInfo_fileShareType' - Undocumented member.
--
-- 'fileShareStatus', 'fileShareInfo_fileShareStatus' - Undocumented member.
--
-- 'fileShareARN', 'fileShareInfo_fileShareARN' - Undocumented member.
--
-- 'gatewayARN', 'fileShareInfo_gatewayARN' - Undocumented member.
newFileShareInfo ::
  FileShareInfo
newFileShareInfo =
  FileShareInfo'
    { fileShareId = Prelude.Nothing,
      fileShareType = Prelude.Nothing,
      fileShareStatus = Prelude.Nothing,
      fileShareARN = Prelude.Nothing,
      gatewayARN = Prelude.Nothing
    }

-- | Undocumented member.
fileShareInfo_fileShareId :: Lens.Lens' FileShareInfo (Prelude.Maybe Prelude.Text)
fileShareInfo_fileShareId = Lens.lens (\FileShareInfo' {fileShareId} -> fileShareId) (\s@FileShareInfo' {} a -> s {fileShareId = a} :: FileShareInfo)

-- | Undocumented member.
fileShareInfo_fileShareType :: Lens.Lens' FileShareInfo (Prelude.Maybe FileShareType)
fileShareInfo_fileShareType = Lens.lens (\FileShareInfo' {fileShareType} -> fileShareType) (\s@FileShareInfo' {} a -> s {fileShareType = a} :: FileShareInfo)

-- | Undocumented member.
fileShareInfo_fileShareStatus :: Lens.Lens' FileShareInfo (Prelude.Maybe Prelude.Text)
fileShareInfo_fileShareStatus = Lens.lens (\FileShareInfo' {fileShareStatus} -> fileShareStatus) (\s@FileShareInfo' {} a -> s {fileShareStatus = a} :: FileShareInfo)

-- | Undocumented member.
fileShareInfo_fileShareARN :: Lens.Lens' FileShareInfo (Prelude.Maybe Prelude.Text)
fileShareInfo_fileShareARN = Lens.lens (\FileShareInfo' {fileShareARN} -> fileShareARN) (\s@FileShareInfo' {} a -> s {fileShareARN = a} :: FileShareInfo)

-- | Undocumented member.
fileShareInfo_gatewayARN :: Lens.Lens' FileShareInfo (Prelude.Maybe Prelude.Text)
fileShareInfo_gatewayARN = Lens.lens (\FileShareInfo' {gatewayARN} -> gatewayARN) (\s@FileShareInfo' {} a -> s {gatewayARN = a} :: FileShareInfo)

instance Prelude.FromJSON FileShareInfo where
  parseJSON =
    Prelude.withObject
      "FileShareInfo"
      ( \x ->
          FileShareInfo'
            Prelude.<$> (x Prelude..:? "FileShareId")
            Prelude.<*> (x Prelude..:? "FileShareType")
            Prelude.<*> (x Prelude..:? "FileShareStatus")
            Prelude.<*> (x Prelude..:? "FileShareARN")
            Prelude.<*> (x Prelude..:? "GatewayARN")
      )

instance Prelude.Hashable FileShareInfo

instance Prelude.NFData FileShareInfo
