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
-- Module      : Amazonka.S3.Types.NotificationConfigurationFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.NotificationConfigurationFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.S3KeyFilter

-- | Specifies object key name filtering rules. For information about key
-- name filtering, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications>
-- in the /Amazon S3 User Guide/.
--
-- /See:/ 'newNotificationConfigurationFilter' smart constructor.
data NotificationConfigurationFilter = NotificationConfigurationFilter'
  { key :: Prelude.Maybe S3KeyFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationConfigurationFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'notificationConfigurationFilter_key' - Undocumented member.
newNotificationConfigurationFilter ::
  NotificationConfigurationFilter
newNotificationConfigurationFilter =
  NotificationConfigurationFilter'
    { key =
        Prelude.Nothing
    }

-- | Undocumented member.
notificationConfigurationFilter_key :: Lens.Lens' NotificationConfigurationFilter (Prelude.Maybe S3KeyFilter)
notificationConfigurationFilter_key = Lens.lens (\NotificationConfigurationFilter' {key} -> key) (\s@NotificationConfigurationFilter' {} a -> s {key = a} :: NotificationConfigurationFilter)

instance Data.FromXML NotificationConfigurationFilter where
  parseXML x =
    NotificationConfigurationFilter'
      Prelude.<$> (x Data..@? "S3Key")

instance
  Prelude.Hashable
    NotificationConfigurationFilter
  where
  hashWithSalt
    _salt
    NotificationConfigurationFilter' {..} =
      _salt `Prelude.hashWithSalt` key

instance
  Prelude.NFData
    NotificationConfigurationFilter
  where
  rnf NotificationConfigurationFilter' {..} =
    Prelude.rnf key

instance Data.ToXML NotificationConfigurationFilter where
  toXML NotificationConfigurationFilter' {..} =
    Prelude.mconcat ["S3Key" Data.@= key]
