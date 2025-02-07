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
-- Module      : Amazonka.IVSChat.Types.RoomSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSChat.Types.RoomSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSChat.Types.MessageReviewHandler
import qualified Amazonka.Prelude as Prelude

-- | Summary information about a room.
--
-- /See:/ 'newRoomSummary' smart constructor.
data RoomSummary = RoomSummary'
  { -- | Room ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Time when the room was created. This is an ISO 8601 timestamp; /note
    -- that this is returned as a string/.
    createTime :: Prelude.Maybe Data.ISO8601,
    -- | Room ID, generated by the system. This is a relative identifier, the
    -- part of the ARN that uniquely identifies the room.
    id :: Prelude.Maybe Prelude.Text,
    -- | List of logging-configuration identifiers attached to the room.
    loggingConfigurationIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | Configuration information for optional review of messages.
    messageReviewHandler :: Prelude.Maybe MessageReviewHandler,
    -- | Room name. The value does not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | Tags attached to the resource. Array of maps, each of the form
    -- @string:string (key:value)@. See
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
    -- for details, including restrictions that apply to tags and \"Tag naming
    -- limits and requirements\"; Amazon IVS Chat has no constraints beyond
    -- what is documented there.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Time of the room’s last update. This is an ISO 8601 timestamp; /note
    -- that this is returned as a string/.
    updateTime :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoomSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'roomSummary_arn' - Room ARN.
--
-- 'createTime', 'roomSummary_createTime' - Time when the room was created. This is an ISO 8601 timestamp; /note
-- that this is returned as a string/.
--
-- 'id', 'roomSummary_id' - Room ID, generated by the system. This is a relative identifier, the
-- part of the ARN that uniquely identifies the room.
--
-- 'loggingConfigurationIdentifiers', 'roomSummary_loggingConfigurationIdentifiers' - List of logging-configuration identifiers attached to the room.
--
-- 'messageReviewHandler', 'roomSummary_messageReviewHandler' - Configuration information for optional review of messages.
--
-- 'name', 'roomSummary_name' - Room name. The value does not need to be unique.
--
-- 'tags', 'roomSummary_tags' - Tags attached to the resource. Array of maps, each of the form
-- @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- for details, including restrictions that apply to tags and \"Tag naming
-- limits and requirements\"; Amazon IVS Chat has no constraints beyond
-- what is documented there.
--
-- 'updateTime', 'roomSummary_updateTime' - Time of the room’s last update. This is an ISO 8601 timestamp; /note
-- that this is returned as a string/.
newRoomSummary ::
  RoomSummary
newRoomSummary =
  RoomSummary'
    { arn = Prelude.Nothing,
      createTime = Prelude.Nothing,
      id = Prelude.Nothing,
      loggingConfigurationIdentifiers = Prelude.Nothing,
      messageReviewHandler = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      updateTime = Prelude.Nothing
    }

-- | Room ARN.
roomSummary_arn :: Lens.Lens' RoomSummary (Prelude.Maybe Prelude.Text)
roomSummary_arn = Lens.lens (\RoomSummary' {arn} -> arn) (\s@RoomSummary' {} a -> s {arn = a} :: RoomSummary)

-- | Time when the room was created. This is an ISO 8601 timestamp; /note
-- that this is returned as a string/.
roomSummary_createTime :: Lens.Lens' RoomSummary (Prelude.Maybe Prelude.UTCTime)
roomSummary_createTime = Lens.lens (\RoomSummary' {createTime} -> createTime) (\s@RoomSummary' {} a -> s {createTime = a} :: RoomSummary) Prelude.. Lens.mapping Data._Time

-- | Room ID, generated by the system. This is a relative identifier, the
-- part of the ARN that uniquely identifies the room.
roomSummary_id :: Lens.Lens' RoomSummary (Prelude.Maybe Prelude.Text)
roomSummary_id = Lens.lens (\RoomSummary' {id} -> id) (\s@RoomSummary' {} a -> s {id = a} :: RoomSummary)

-- | List of logging-configuration identifiers attached to the room.
roomSummary_loggingConfigurationIdentifiers :: Lens.Lens' RoomSummary (Prelude.Maybe [Prelude.Text])
roomSummary_loggingConfigurationIdentifiers = Lens.lens (\RoomSummary' {loggingConfigurationIdentifiers} -> loggingConfigurationIdentifiers) (\s@RoomSummary' {} a -> s {loggingConfigurationIdentifiers = a} :: RoomSummary) Prelude.. Lens.mapping Lens.coerced

-- | Configuration information for optional review of messages.
roomSummary_messageReviewHandler :: Lens.Lens' RoomSummary (Prelude.Maybe MessageReviewHandler)
roomSummary_messageReviewHandler = Lens.lens (\RoomSummary' {messageReviewHandler} -> messageReviewHandler) (\s@RoomSummary' {} a -> s {messageReviewHandler = a} :: RoomSummary)

-- | Room name. The value does not need to be unique.
roomSummary_name :: Lens.Lens' RoomSummary (Prelude.Maybe Prelude.Text)
roomSummary_name = Lens.lens (\RoomSummary' {name} -> name) (\s@RoomSummary' {} a -> s {name = a} :: RoomSummary)

-- | Tags attached to the resource. Array of maps, each of the form
-- @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- for details, including restrictions that apply to tags and \"Tag naming
-- limits and requirements\"; Amazon IVS Chat has no constraints beyond
-- what is documented there.
roomSummary_tags :: Lens.Lens' RoomSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
roomSummary_tags = Lens.lens (\RoomSummary' {tags} -> tags) (\s@RoomSummary' {} a -> s {tags = a} :: RoomSummary) Prelude.. Lens.mapping Lens.coerced

-- | Time of the room’s last update. This is an ISO 8601 timestamp; /note
-- that this is returned as a string/.
roomSummary_updateTime :: Lens.Lens' RoomSummary (Prelude.Maybe Prelude.UTCTime)
roomSummary_updateTime = Lens.lens (\RoomSummary' {updateTime} -> updateTime) (\s@RoomSummary' {} a -> s {updateTime = a} :: RoomSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON RoomSummary where
  parseJSON =
    Data.withObject
      "RoomSummary"
      ( \x ->
          RoomSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createTime")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> ( x
                            Data..:? "loggingConfigurationIdentifiers"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "messageReviewHandler")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "updateTime")
      )

instance Prelude.Hashable RoomSummary where
  hashWithSalt _salt RoomSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` loggingConfigurationIdentifiers
      `Prelude.hashWithSalt` messageReviewHandler
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData RoomSummary where
  rnf RoomSummary' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf createTime `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf loggingConfigurationIdentifiers `Prelude.seq`
            Prelude.rnf messageReviewHandler `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf tags `Prelude.seq`
                  Prelude.rnf updateTime
