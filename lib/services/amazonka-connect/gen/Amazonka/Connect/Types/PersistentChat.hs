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
-- Module      : Amazonka.Connect.Types.PersistentChat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.PersistentChat where

import Amazonka.Connect.Types.RehydrationType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Enable persistent chats. For more information about enabling persistent
-- chat, and for example use cases and how to configure for them, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/chat-persistence.html Enable persistent chat>.
--
-- /See:/ 'newPersistentChat' smart constructor.
data PersistentChat = PersistentChat'
  { -- | The contactId that is used for rehydration depends on the rehydration
    -- type. RehydrationType is required for persistent chat.
    --
    -- -   @ENTIRE_PAST_SESSION@: Rehydrates a chat from the most recently
    --     terminated past chat contact of the specified past ended chat
    --     session. To use this type, provide the @initialContactId@ of the
    --     past ended chat session in the @sourceContactId@ field. In this
    --     type, Amazon Connect determines the most recent chat contact on the
    --     specified chat session that has ended, and uses it to start a
    --     persistent chat.
    --
    -- -   @FROM_SEGMENT@: Rehydrates a chat from the past chat contact that is
    --     specified in the @sourceContactId@ field.
    --
    -- The actual contactId used for rehydration is provided in the response of
    -- this API.
    rehydrationType :: Prelude.Maybe RehydrationType,
    -- | The contactId from which a persistent chat session must be started.
    sourceContactId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PersistentChat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rehydrationType', 'persistentChat_rehydrationType' - The contactId that is used for rehydration depends on the rehydration
-- type. RehydrationType is required for persistent chat.
--
-- -   @ENTIRE_PAST_SESSION@: Rehydrates a chat from the most recently
--     terminated past chat contact of the specified past ended chat
--     session. To use this type, provide the @initialContactId@ of the
--     past ended chat session in the @sourceContactId@ field. In this
--     type, Amazon Connect determines the most recent chat contact on the
--     specified chat session that has ended, and uses it to start a
--     persistent chat.
--
-- -   @FROM_SEGMENT@: Rehydrates a chat from the past chat contact that is
--     specified in the @sourceContactId@ field.
--
-- The actual contactId used for rehydration is provided in the response of
-- this API.
--
-- 'sourceContactId', 'persistentChat_sourceContactId' - The contactId from which a persistent chat session must be started.
newPersistentChat ::
  PersistentChat
newPersistentChat =
  PersistentChat'
    { rehydrationType = Prelude.Nothing,
      sourceContactId = Prelude.Nothing
    }

-- | The contactId that is used for rehydration depends on the rehydration
-- type. RehydrationType is required for persistent chat.
--
-- -   @ENTIRE_PAST_SESSION@: Rehydrates a chat from the most recently
--     terminated past chat contact of the specified past ended chat
--     session. To use this type, provide the @initialContactId@ of the
--     past ended chat session in the @sourceContactId@ field. In this
--     type, Amazon Connect determines the most recent chat contact on the
--     specified chat session that has ended, and uses it to start a
--     persistent chat.
--
-- -   @FROM_SEGMENT@: Rehydrates a chat from the past chat contact that is
--     specified in the @sourceContactId@ field.
--
-- The actual contactId used for rehydration is provided in the response of
-- this API.
persistentChat_rehydrationType :: Lens.Lens' PersistentChat (Prelude.Maybe RehydrationType)
persistentChat_rehydrationType = Lens.lens (\PersistentChat' {rehydrationType} -> rehydrationType) (\s@PersistentChat' {} a -> s {rehydrationType = a} :: PersistentChat)

-- | The contactId from which a persistent chat session must be started.
persistentChat_sourceContactId :: Lens.Lens' PersistentChat (Prelude.Maybe Prelude.Text)
persistentChat_sourceContactId = Lens.lens (\PersistentChat' {sourceContactId} -> sourceContactId) (\s@PersistentChat' {} a -> s {sourceContactId = a} :: PersistentChat)

instance Prelude.Hashable PersistentChat where
  hashWithSalt _salt PersistentChat' {..} =
    _salt
      `Prelude.hashWithSalt` rehydrationType
      `Prelude.hashWithSalt` sourceContactId

instance Prelude.NFData PersistentChat where
  rnf PersistentChat' {..} =
    Prelude.rnf rehydrationType
      `Prelude.seq` Prelude.rnf sourceContactId

instance Data.ToJSON PersistentChat where
  toJSON PersistentChat' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RehydrationType" Data..=)
              Prelude.<$> rehydrationType,
            ("SourceContactId" Data..=)
              Prelude.<$> sourceContactId
          ]
      )
