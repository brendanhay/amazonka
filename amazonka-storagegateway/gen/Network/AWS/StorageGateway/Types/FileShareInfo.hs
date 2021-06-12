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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.StorageGateway.Types.FileShareType

-- | Describes a file share.
--
-- /See:/ 'newFileShareInfo' smart constructor.
data FileShareInfo = FileShareInfo'
  { fileShareId :: Core.Maybe Core.Text,
    fileShareType :: Core.Maybe FileShareType,
    fileShareStatus :: Core.Maybe Core.Text,
    fileShareARN :: Core.Maybe Core.Text,
    gatewayARN :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { fileShareId = Core.Nothing,
      fileShareType = Core.Nothing,
      fileShareStatus = Core.Nothing,
      fileShareARN = Core.Nothing,
      gatewayARN = Core.Nothing
    }

-- | Undocumented member.
fileShareInfo_fileShareId :: Lens.Lens' FileShareInfo (Core.Maybe Core.Text)
fileShareInfo_fileShareId = Lens.lens (\FileShareInfo' {fileShareId} -> fileShareId) (\s@FileShareInfo' {} a -> s {fileShareId = a} :: FileShareInfo)

-- | Undocumented member.
fileShareInfo_fileShareType :: Lens.Lens' FileShareInfo (Core.Maybe FileShareType)
fileShareInfo_fileShareType = Lens.lens (\FileShareInfo' {fileShareType} -> fileShareType) (\s@FileShareInfo' {} a -> s {fileShareType = a} :: FileShareInfo)

-- | Undocumented member.
fileShareInfo_fileShareStatus :: Lens.Lens' FileShareInfo (Core.Maybe Core.Text)
fileShareInfo_fileShareStatus = Lens.lens (\FileShareInfo' {fileShareStatus} -> fileShareStatus) (\s@FileShareInfo' {} a -> s {fileShareStatus = a} :: FileShareInfo)

-- | Undocumented member.
fileShareInfo_fileShareARN :: Lens.Lens' FileShareInfo (Core.Maybe Core.Text)
fileShareInfo_fileShareARN = Lens.lens (\FileShareInfo' {fileShareARN} -> fileShareARN) (\s@FileShareInfo' {} a -> s {fileShareARN = a} :: FileShareInfo)

-- | Undocumented member.
fileShareInfo_gatewayARN :: Lens.Lens' FileShareInfo (Core.Maybe Core.Text)
fileShareInfo_gatewayARN = Lens.lens (\FileShareInfo' {gatewayARN} -> gatewayARN) (\s@FileShareInfo' {} a -> s {gatewayARN = a} :: FileShareInfo)

instance Core.FromJSON FileShareInfo where
  parseJSON =
    Core.withObject
      "FileShareInfo"
      ( \x ->
          FileShareInfo'
            Core.<$> (x Core..:? "FileShareId")
            Core.<*> (x Core..:? "FileShareType")
            Core.<*> (x Core..:? "FileShareStatus")
            Core.<*> (x Core..:? "FileShareARN")
            Core.<*> (x Core..:? "GatewayARN")
      )

instance Core.Hashable FileShareInfo

instance Core.NFData FileShareInfo
