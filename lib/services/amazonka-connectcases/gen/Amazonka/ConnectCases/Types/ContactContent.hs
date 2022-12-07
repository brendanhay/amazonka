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
-- Module      : Amazonka.ConnectCases.Types.ContactContent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.ContactContent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a content of an Amazon Connect contact object.
--
-- /See:/ 'newContactContent' smart constructor.
data ContactContent = ContactContent'
  { -- | A list of channels to filter on for related items of type @Contact@.
    channel :: Prelude.Text,
    -- | The difference between the @InitiationTimestamp@ and the
    -- @DisconnectTimestamp@ of the contact.
    connectedToSystemTime :: Data.POSIX,
    -- | A unique identifier of a contact in Amazon Connect.
    contactArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContactContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channel', 'contactContent_channel' - A list of channels to filter on for related items of type @Contact@.
--
-- 'connectedToSystemTime', 'contactContent_connectedToSystemTime' - The difference between the @InitiationTimestamp@ and the
-- @DisconnectTimestamp@ of the contact.
--
-- 'contactArn', 'contactContent_contactArn' - A unique identifier of a contact in Amazon Connect.
newContactContent ::
  -- | 'channel'
  Prelude.Text ->
  -- | 'connectedToSystemTime'
  Prelude.UTCTime ->
  -- | 'contactArn'
  Prelude.Text ->
  ContactContent
newContactContent
  pChannel_
  pConnectedToSystemTime_
  pContactArn_ =
    ContactContent'
      { channel = pChannel_,
        connectedToSystemTime =
          Data._Time Lens.# pConnectedToSystemTime_,
        contactArn = pContactArn_
      }

-- | A list of channels to filter on for related items of type @Contact@.
contactContent_channel :: Lens.Lens' ContactContent Prelude.Text
contactContent_channel = Lens.lens (\ContactContent' {channel} -> channel) (\s@ContactContent' {} a -> s {channel = a} :: ContactContent)

-- | The difference between the @InitiationTimestamp@ and the
-- @DisconnectTimestamp@ of the contact.
contactContent_connectedToSystemTime :: Lens.Lens' ContactContent Prelude.UTCTime
contactContent_connectedToSystemTime = Lens.lens (\ContactContent' {connectedToSystemTime} -> connectedToSystemTime) (\s@ContactContent' {} a -> s {connectedToSystemTime = a} :: ContactContent) Prelude.. Data._Time

-- | A unique identifier of a contact in Amazon Connect.
contactContent_contactArn :: Lens.Lens' ContactContent Prelude.Text
contactContent_contactArn = Lens.lens (\ContactContent' {contactArn} -> contactArn) (\s@ContactContent' {} a -> s {contactArn = a} :: ContactContent)

instance Data.FromJSON ContactContent where
  parseJSON =
    Data.withObject
      "ContactContent"
      ( \x ->
          ContactContent'
            Prelude.<$> (x Data..: "channel")
            Prelude.<*> (x Data..: "connectedToSystemTime")
            Prelude.<*> (x Data..: "contactArn")
      )

instance Prelude.Hashable ContactContent where
  hashWithSalt _salt ContactContent' {..} =
    _salt `Prelude.hashWithSalt` channel
      `Prelude.hashWithSalt` connectedToSystemTime
      `Prelude.hashWithSalt` contactArn

instance Prelude.NFData ContactContent where
  rnf ContactContent' {..} =
    Prelude.rnf channel
      `Prelude.seq` Prelude.rnf connectedToSystemTime
      `Prelude.seq` Prelude.rnf contactArn
