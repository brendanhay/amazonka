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
-- Module      : Amazonka.ECS.Types.Attachment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.Attachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.KeyValuePair
import qualified Amazonka.Prelude as Prelude

-- | An object representing a container instance or task attachment.
--
-- /See:/ 'newAttachment' smart constructor.
data Attachment = Attachment'
  { -- | The type of the attachment, such as @ElasticNetworkInterface@.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The status of the attachment. Valid values are @PRECREATED@, @CREATED@,
    -- @ATTACHING@, @ATTACHED@, @DETACHING@, @DETACHED@, @DELETED@, and
    -- @FAILED@.
    status :: Prelude.Maybe Prelude.Text,
    -- | Details of the attachment. For elastic network interfaces, this includes
    -- the network interface ID, the MAC address, the subnet ID, and the
    -- private IPv4 address.
    details :: Prelude.Maybe [KeyValuePair],
    -- | The unique identifier for the attachment.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Attachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'attachment_type' - The type of the attachment, such as @ElasticNetworkInterface@.
--
-- 'status', 'attachment_status' - The status of the attachment. Valid values are @PRECREATED@, @CREATED@,
-- @ATTACHING@, @ATTACHED@, @DETACHING@, @DETACHED@, @DELETED@, and
-- @FAILED@.
--
-- 'details', 'attachment_details' - Details of the attachment. For elastic network interfaces, this includes
-- the network interface ID, the MAC address, the subnet ID, and the
-- private IPv4 address.
--
-- 'id', 'attachment_id' - The unique identifier for the attachment.
newAttachment ::
  Attachment
newAttachment =
  Attachment'
    { type' = Prelude.Nothing,
      status = Prelude.Nothing,
      details = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The type of the attachment, such as @ElasticNetworkInterface@.
attachment_type :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_type = Lens.lens (\Attachment' {type'} -> type') (\s@Attachment' {} a -> s {type' = a} :: Attachment)

-- | The status of the attachment. Valid values are @PRECREATED@, @CREATED@,
-- @ATTACHING@, @ATTACHED@, @DETACHING@, @DETACHED@, @DELETED@, and
-- @FAILED@.
attachment_status :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_status = Lens.lens (\Attachment' {status} -> status) (\s@Attachment' {} a -> s {status = a} :: Attachment)

-- | Details of the attachment. For elastic network interfaces, this includes
-- the network interface ID, the MAC address, the subnet ID, and the
-- private IPv4 address.
attachment_details :: Lens.Lens' Attachment (Prelude.Maybe [KeyValuePair])
attachment_details = Lens.lens (\Attachment' {details} -> details) (\s@Attachment' {} a -> s {details = a} :: Attachment) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier for the attachment.
attachment_id :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_id = Lens.lens (\Attachment' {id} -> id) (\s@Attachment' {} a -> s {id = a} :: Attachment)

instance Data.FromJSON Attachment where
  parseJSON =
    Data.withObject
      "Attachment"
      ( \x ->
          Attachment'
            Prelude.<$> (x Data..:? "type")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "details" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "id")
      )

instance Prelude.Hashable Attachment where
  hashWithSalt _salt Attachment' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` id

instance Prelude.NFData Attachment where
  rnf Attachment' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf id
