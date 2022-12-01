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
-- Module      : Amazonka.SSMIncidents.Types.NotificationTargetItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.NotificationTargetItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The SNS targets that are notified when updates are made to an incident.
--
-- /See:/ 'newNotificationTargetItem' smart constructor.
data NotificationTargetItem = NotificationTargetItem'
  { -- | The Amazon Resource Name (ARN) of the SNS topic.
    snsTopicArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationTargetItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snsTopicArn', 'notificationTargetItem_snsTopicArn' - The Amazon Resource Name (ARN) of the SNS topic.
newNotificationTargetItem ::
  NotificationTargetItem
newNotificationTargetItem =
  NotificationTargetItem'
    { snsTopicArn =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the SNS topic.
notificationTargetItem_snsTopicArn :: Lens.Lens' NotificationTargetItem (Prelude.Maybe Prelude.Text)
notificationTargetItem_snsTopicArn = Lens.lens (\NotificationTargetItem' {snsTopicArn} -> snsTopicArn) (\s@NotificationTargetItem' {} a -> s {snsTopicArn = a} :: NotificationTargetItem)

instance Core.FromJSON NotificationTargetItem where
  parseJSON =
    Core.withObject
      "NotificationTargetItem"
      ( \x ->
          NotificationTargetItem'
            Prelude.<$> (x Core..:? "snsTopicArn")
      )

instance Prelude.Hashable NotificationTargetItem where
  hashWithSalt _salt NotificationTargetItem' {..} =
    _salt `Prelude.hashWithSalt` snsTopicArn

instance Prelude.NFData NotificationTargetItem where
  rnf NotificationTargetItem' {..} =
    Prelude.rnf snsTopicArn

instance Core.ToJSON NotificationTargetItem where
  toJSON NotificationTargetItem' {..} =
    Core.object
      ( Prelude.catMaybes
          [("snsTopicArn" Core..=) Prelude.<$> snsTopicArn]
      )
