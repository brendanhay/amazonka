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
-- Module      : Network.AWS.ECS.Types.Attachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Attachment where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.KeyValuePair
import qualified Network.AWS.Lens as Lens

-- | An object representing a container instance or task attachment.
--
-- /See:/ 'newAttachment' smart constructor.
data Attachment = Attachment'
  { -- | The status of the attachment. Valid values are @PRECREATED@, @CREATED@,
    -- @ATTACHING@, @ATTACHED@, @DETACHING@, @DETACHED@, and @DELETED@.
    status :: Core.Maybe Core.Text,
    -- | The unique identifier for the attachment.
    id :: Core.Maybe Core.Text,
    -- | Details of the attachment. For elastic network interfaces, this includes
    -- the network interface ID, the MAC address, the subnet ID, and the
    -- private IPv4 address.
    details :: Core.Maybe [KeyValuePair],
    -- | The type of the attachment, such as @ElasticNetworkInterface@.
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Attachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'attachment_status' - The status of the attachment. Valid values are @PRECREATED@, @CREATED@,
-- @ATTACHING@, @ATTACHED@, @DETACHING@, @DETACHED@, and @DELETED@.
--
-- 'id', 'attachment_id' - The unique identifier for the attachment.
--
-- 'details', 'attachment_details' - Details of the attachment. For elastic network interfaces, this includes
-- the network interface ID, the MAC address, the subnet ID, and the
-- private IPv4 address.
--
-- 'type'', 'attachment_type' - The type of the attachment, such as @ElasticNetworkInterface@.
newAttachment ::
  Attachment
newAttachment =
  Attachment'
    { status = Core.Nothing,
      id = Core.Nothing,
      details = Core.Nothing,
      type' = Core.Nothing
    }

-- | The status of the attachment. Valid values are @PRECREATED@, @CREATED@,
-- @ATTACHING@, @ATTACHED@, @DETACHING@, @DETACHED@, and @DELETED@.
attachment_status :: Lens.Lens' Attachment (Core.Maybe Core.Text)
attachment_status = Lens.lens (\Attachment' {status} -> status) (\s@Attachment' {} a -> s {status = a} :: Attachment)

-- | The unique identifier for the attachment.
attachment_id :: Lens.Lens' Attachment (Core.Maybe Core.Text)
attachment_id = Lens.lens (\Attachment' {id} -> id) (\s@Attachment' {} a -> s {id = a} :: Attachment)

-- | Details of the attachment. For elastic network interfaces, this includes
-- the network interface ID, the MAC address, the subnet ID, and the
-- private IPv4 address.
attachment_details :: Lens.Lens' Attachment (Core.Maybe [KeyValuePair])
attachment_details = Lens.lens (\Attachment' {details} -> details) (\s@Attachment' {} a -> s {details = a} :: Attachment) Core.. Lens.mapping Lens._Coerce

-- | The type of the attachment, such as @ElasticNetworkInterface@.
attachment_type :: Lens.Lens' Attachment (Core.Maybe Core.Text)
attachment_type = Lens.lens (\Attachment' {type'} -> type') (\s@Attachment' {} a -> s {type' = a} :: Attachment)

instance Core.FromJSON Attachment where
  parseJSON =
    Core.withObject
      "Attachment"
      ( \x ->
          Attachment'
            Core.<$> (x Core..:? "status")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "details" Core..!= Core.mempty)
            Core.<*> (x Core..:? "type")
      )

instance Core.Hashable Attachment

instance Core.NFData Attachment
