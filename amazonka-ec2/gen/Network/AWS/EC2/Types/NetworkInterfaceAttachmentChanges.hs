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
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceAttachmentChanges
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceAttachmentChanges where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes an attachment change.
--
-- /See:/ 'newNetworkInterfaceAttachmentChanges' smart constructor.
data NetworkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges'
  { -- | The ID of the network interface attachment.
    attachmentId :: Core.Maybe Core.Text,
    -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NetworkInterfaceAttachmentChanges' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentId', 'networkInterfaceAttachmentChanges_attachmentId' - The ID of the network interface attachment.
--
-- 'deleteOnTermination', 'networkInterfaceAttachmentChanges_deleteOnTermination' - Indicates whether the network interface is deleted when the instance is
-- terminated.
newNetworkInterfaceAttachmentChanges ::
  NetworkInterfaceAttachmentChanges
newNetworkInterfaceAttachmentChanges =
  NetworkInterfaceAttachmentChanges'
    { attachmentId =
        Core.Nothing,
      deleteOnTermination = Core.Nothing
    }

-- | The ID of the network interface attachment.
networkInterfaceAttachmentChanges_attachmentId :: Lens.Lens' NetworkInterfaceAttachmentChanges (Core.Maybe Core.Text)
networkInterfaceAttachmentChanges_attachmentId = Lens.lens (\NetworkInterfaceAttachmentChanges' {attachmentId} -> attachmentId) (\s@NetworkInterfaceAttachmentChanges' {} a -> s {attachmentId = a} :: NetworkInterfaceAttachmentChanges)

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
networkInterfaceAttachmentChanges_deleteOnTermination :: Lens.Lens' NetworkInterfaceAttachmentChanges (Core.Maybe Core.Bool)
networkInterfaceAttachmentChanges_deleteOnTermination = Lens.lens (\NetworkInterfaceAttachmentChanges' {deleteOnTermination} -> deleteOnTermination) (\s@NetworkInterfaceAttachmentChanges' {} a -> s {deleteOnTermination = a} :: NetworkInterfaceAttachmentChanges)

instance
  Core.Hashable
    NetworkInterfaceAttachmentChanges

instance
  Core.NFData
    NetworkInterfaceAttachmentChanges

instance
  Core.ToQuery
    NetworkInterfaceAttachmentChanges
  where
  toQuery NetworkInterfaceAttachmentChanges' {..} =
    Core.mconcat
      [ "AttachmentId" Core.=: attachmentId,
        "DeleteOnTermination" Core.=: deleteOnTermination
      ]
