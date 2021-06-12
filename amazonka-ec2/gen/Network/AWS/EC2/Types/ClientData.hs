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
-- Module      : Network.AWS.EC2.Types.ClientData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientData where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the client-specific data.
--
-- /See:/ 'newClientData' smart constructor.
data ClientData = ClientData'
  { -- | The time that the disk upload ends.
    uploadEnd :: Core.Maybe Core.ISO8601,
    -- | A user-defined comment about the disk upload.
    comment :: Core.Maybe Core.Text,
    -- | The size of the uploaded disk image, in GiB.
    uploadSize :: Core.Maybe Core.Double,
    -- | The time that the disk upload starts.
    uploadStart :: Core.Maybe Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClientData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uploadEnd', 'clientData_uploadEnd' - The time that the disk upload ends.
--
-- 'comment', 'clientData_comment' - A user-defined comment about the disk upload.
--
-- 'uploadSize', 'clientData_uploadSize' - The size of the uploaded disk image, in GiB.
--
-- 'uploadStart', 'clientData_uploadStart' - The time that the disk upload starts.
newClientData ::
  ClientData
newClientData =
  ClientData'
    { uploadEnd = Core.Nothing,
      comment = Core.Nothing,
      uploadSize = Core.Nothing,
      uploadStart = Core.Nothing
    }

-- | The time that the disk upload ends.
clientData_uploadEnd :: Lens.Lens' ClientData (Core.Maybe Core.UTCTime)
clientData_uploadEnd = Lens.lens (\ClientData' {uploadEnd} -> uploadEnd) (\s@ClientData' {} a -> s {uploadEnd = a} :: ClientData) Core.. Lens.mapping Core._Time

-- | A user-defined comment about the disk upload.
clientData_comment :: Lens.Lens' ClientData (Core.Maybe Core.Text)
clientData_comment = Lens.lens (\ClientData' {comment} -> comment) (\s@ClientData' {} a -> s {comment = a} :: ClientData)

-- | The size of the uploaded disk image, in GiB.
clientData_uploadSize :: Lens.Lens' ClientData (Core.Maybe Core.Double)
clientData_uploadSize = Lens.lens (\ClientData' {uploadSize} -> uploadSize) (\s@ClientData' {} a -> s {uploadSize = a} :: ClientData)

-- | The time that the disk upload starts.
clientData_uploadStart :: Lens.Lens' ClientData (Core.Maybe Core.UTCTime)
clientData_uploadStart = Lens.lens (\ClientData' {uploadStart} -> uploadStart) (\s@ClientData' {} a -> s {uploadStart = a} :: ClientData) Core.. Lens.mapping Core._Time

instance Core.Hashable ClientData

instance Core.NFData ClientData

instance Core.ToQuery ClientData where
  toQuery ClientData' {..} =
    Core.mconcat
      [ "UploadEnd" Core.=: uploadEnd,
        "Comment" Core.=: comment,
        "UploadSize" Core.=: uploadSize,
        "UploadStart" Core.=: uploadStart
      ]
