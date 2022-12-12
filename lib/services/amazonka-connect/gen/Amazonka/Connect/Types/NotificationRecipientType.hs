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
-- Module      : Amazonka.Connect.Types.NotificationRecipientType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.NotificationRecipientType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The type of notification recipient.
--
-- /See:/ 'newNotificationRecipientType' smart constructor.
data NotificationRecipientType = NotificationRecipientType'
  { -- | A list of user IDs.
    userIds :: Prelude.Maybe [Prelude.Text],
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    -- Amazon Connect users with the specified tags will be notified.
    userTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationRecipientType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userIds', 'notificationRecipientType_userIds' - A list of user IDs.
--
-- 'userTags', 'notificationRecipientType_userTags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
-- Amazon Connect users with the specified tags will be notified.
newNotificationRecipientType ::
  NotificationRecipientType
newNotificationRecipientType =
  NotificationRecipientType'
    { userIds =
        Prelude.Nothing,
      userTags = Prelude.Nothing
    }

-- | A list of user IDs.
notificationRecipientType_userIds :: Lens.Lens' NotificationRecipientType (Prelude.Maybe [Prelude.Text])
notificationRecipientType_userIds = Lens.lens (\NotificationRecipientType' {userIds} -> userIds) (\s@NotificationRecipientType' {} a -> s {userIds = a} :: NotificationRecipientType) Prelude.. Lens.mapping Lens.coerced

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
-- Amazon Connect users with the specified tags will be notified.
notificationRecipientType_userTags :: Lens.Lens' NotificationRecipientType (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
notificationRecipientType_userTags = Lens.lens (\NotificationRecipientType' {userTags} -> userTags) (\s@NotificationRecipientType' {} a -> s {userTags = a} :: NotificationRecipientType) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON NotificationRecipientType where
  parseJSON =
    Data.withObject
      "NotificationRecipientType"
      ( \x ->
          NotificationRecipientType'
            Prelude.<$> (x Data..:? "UserIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "UserTags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable NotificationRecipientType where
  hashWithSalt _salt NotificationRecipientType' {..} =
    _salt `Prelude.hashWithSalt` userIds
      `Prelude.hashWithSalt` userTags

instance Prelude.NFData NotificationRecipientType where
  rnf NotificationRecipientType' {..} =
    Prelude.rnf userIds
      `Prelude.seq` Prelude.rnf userTags

instance Data.ToJSON NotificationRecipientType where
  toJSON NotificationRecipientType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("UserIds" Data..=) Prelude.<$> userIds,
            ("UserTags" Data..=) Prelude.<$> userTags
          ]
      )
