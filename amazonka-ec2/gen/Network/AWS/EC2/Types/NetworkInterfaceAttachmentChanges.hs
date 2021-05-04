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
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceAttachmentChanges
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceAttachmentChanges where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an attachment change.
--
-- /See:/ 'newNetworkInterfaceAttachmentChanges' smart constructor.
data NetworkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges'
  { -- | The ID of the network interface attachment.
    attachmentId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      deleteOnTermination = Prelude.Nothing
    }

-- | The ID of the network interface attachment.
networkInterfaceAttachmentChanges_attachmentId :: Lens.Lens' NetworkInterfaceAttachmentChanges (Prelude.Maybe Prelude.Text)
networkInterfaceAttachmentChanges_attachmentId = Lens.lens (\NetworkInterfaceAttachmentChanges' {attachmentId} -> attachmentId) (\s@NetworkInterfaceAttachmentChanges' {} a -> s {attachmentId = a} :: NetworkInterfaceAttachmentChanges)

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
networkInterfaceAttachmentChanges_deleteOnTermination :: Lens.Lens' NetworkInterfaceAttachmentChanges (Prelude.Maybe Prelude.Bool)
networkInterfaceAttachmentChanges_deleteOnTermination = Lens.lens (\NetworkInterfaceAttachmentChanges' {deleteOnTermination} -> deleteOnTermination) (\s@NetworkInterfaceAttachmentChanges' {} a -> s {deleteOnTermination = a} :: NetworkInterfaceAttachmentChanges)

instance
  Prelude.Hashable
    NetworkInterfaceAttachmentChanges

instance
  Prelude.NFData
    NetworkInterfaceAttachmentChanges

instance
  Prelude.ToQuery
    NetworkInterfaceAttachmentChanges
  where
  toQuery NetworkInterfaceAttachmentChanges' {..} =
    Prelude.mconcat
      [ "AttachmentId" Prelude.=: attachmentId,
        "DeleteOnTermination" Prelude.=: deleteOnTermination
      ]
