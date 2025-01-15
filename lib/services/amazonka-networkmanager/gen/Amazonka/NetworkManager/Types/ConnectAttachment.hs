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
-- Module      : Amazonka.NetworkManager.Types.ConnectAttachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.ConnectAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.Attachment
import Amazonka.NetworkManager.Types.ConnectAttachmentOptions
import qualified Amazonka.Prelude as Prelude

-- | Describes a core network Connect attachment.
--
-- /See:/ 'newConnectAttachment' smart constructor.
data ConnectAttachment = ConnectAttachment'
  { -- | The attachment details.
    attachment :: Prelude.Maybe Attachment,
    -- | Options for connecting an attachment.
    options :: Prelude.Maybe ConnectAttachmentOptions,
    -- | The ID of the transport attachment.
    transportAttachmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachment', 'connectAttachment_attachment' - The attachment details.
--
-- 'options', 'connectAttachment_options' - Options for connecting an attachment.
--
-- 'transportAttachmentId', 'connectAttachment_transportAttachmentId' - The ID of the transport attachment.
newConnectAttachment ::
  ConnectAttachment
newConnectAttachment =
  ConnectAttachment'
    { attachment = Prelude.Nothing,
      options = Prelude.Nothing,
      transportAttachmentId = Prelude.Nothing
    }

-- | The attachment details.
connectAttachment_attachment :: Lens.Lens' ConnectAttachment (Prelude.Maybe Attachment)
connectAttachment_attachment = Lens.lens (\ConnectAttachment' {attachment} -> attachment) (\s@ConnectAttachment' {} a -> s {attachment = a} :: ConnectAttachment)

-- | Options for connecting an attachment.
connectAttachment_options :: Lens.Lens' ConnectAttachment (Prelude.Maybe ConnectAttachmentOptions)
connectAttachment_options = Lens.lens (\ConnectAttachment' {options} -> options) (\s@ConnectAttachment' {} a -> s {options = a} :: ConnectAttachment)

-- | The ID of the transport attachment.
connectAttachment_transportAttachmentId :: Lens.Lens' ConnectAttachment (Prelude.Maybe Prelude.Text)
connectAttachment_transportAttachmentId = Lens.lens (\ConnectAttachment' {transportAttachmentId} -> transportAttachmentId) (\s@ConnectAttachment' {} a -> s {transportAttachmentId = a} :: ConnectAttachment)

instance Data.FromJSON ConnectAttachment where
  parseJSON =
    Data.withObject
      "ConnectAttachment"
      ( \x ->
          ConnectAttachment'
            Prelude.<$> (x Data..:? "Attachment")
            Prelude.<*> (x Data..:? "Options")
            Prelude.<*> (x Data..:? "TransportAttachmentId")
      )

instance Prelude.Hashable ConnectAttachment where
  hashWithSalt _salt ConnectAttachment' {..} =
    _salt
      `Prelude.hashWithSalt` attachment
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` transportAttachmentId

instance Prelude.NFData ConnectAttachment where
  rnf ConnectAttachment' {..} =
    Prelude.rnf attachment `Prelude.seq`
      Prelude.rnf options `Prelude.seq`
        Prelude.rnf transportAttachmentId
