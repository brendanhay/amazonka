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
-- Module      : Network.AWS.ECS.Types.Attachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Attachment where

import Network.AWS.ECS.Types.KeyValuePair
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing a container instance or task attachment.
--
-- /See:/ 'newAttachment' smart constructor.
data Attachment = Attachment'
  { -- | The status of the attachment. Valid values are @PRECREATED@, @CREATED@,
    -- @ATTACHING@, @ATTACHED@, @DETACHING@, @DETACHED@, and @DELETED@.
    status :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the attachment.
    id :: Prelude.Maybe Prelude.Text,
    -- | Details of the attachment. For elastic network interfaces, this includes
    -- the network interface ID, the MAC address, the subnet ID, and the
    -- private IPv4 address.
    details :: Prelude.Maybe [KeyValuePair],
    -- | The type of the attachment, such as @ElasticNetworkInterface@.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { status = Prelude.Nothing,
      id = Prelude.Nothing,
      details = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The status of the attachment. Valid values are @PRECREATED@, @CREATED@,
-- @ATTACHING@, @ATTACHED@, @DETACHING@, @DETACHED@, and @DELETED@.
attachment_status :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_status = Lens.lens (\Attachment' {status} -> status) (\s@Attachment' {} a -> s {status = a} :: Attachment)

-- | The unique identifier for the attachment.
attachment_id :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_id = Lens.lens (\Attachment' {id} -> id) (\s@Attachment' {} a -> s {id = a} :: Attachment)

-- | Details of the attachment. For elastic network interfaces, this includes
-- the network interface ID, the MAC address, the subnet ID, and the
-- private IPv4 address.
attachment_details :: Lens.Lens' Attachment (Prelude.Maybe [KeyValuePair])
attachment_details = Lens.lens (\Attachment' {details} -> details) (\s@Attachment' {} a -> s {details = a} :: Attachment) Prelude.. Lens.mapping Prelude._Coerce

-- | The type of the attachment, such as @ElasticNetworkInterface@.
attachment_type :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_type = Lens.lens (\Attachment' {type'} -> type') (\s@Attachment' {} a -> s {type' = a} :: Attachment)

instance Prelude.FromJSON Attachment where
  parseJSON =
    Prelude.withObject
      "Attachment"
      ( \x ->
          Attachment'
            Prelude.<$> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "id")
            Prelude.<*> (x Prelude..:? "details" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "type")
      )

instance Prelude.Hashable Attachment

instance Prelude.NFData Attachment
