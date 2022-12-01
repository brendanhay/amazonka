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
-- Module      : Amazonka.KinesisVideo.Types.NotificationDestinationConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.NotificationDestinationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The structure that contains the information required to deliver a
-- notification to a customer.
--
-- /See:/ 'newNotificationDestinationConfig' smart constructor.
data NotificationDestinationConfig = NotificationDestinationConfig'
  { -- | The Uniform Resource Idenifier (URI) that identifies where the images
    -- will be delivered.
    uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationDestinationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uri', 'notificationDestinationConfig_uri' - The Uniform Resource Idenifier (URI) that identifies where the images
-- will be delivered.
newNotificationDestinationConfig ::
  -- | 'uri'
  Prelude.Text ->
  NotificationDestinationConfig
newNotificationDestinationConfig pUri_ =
  NotificationDestinationConfig' {uri = pUri_}

-- | The Uniform Resource Idenifier (URI) that identifies where the images
-- will be delivered.
notificationDestinationConfig_uri :: Lens.Lens' NotificationDestinationConfig Prelude.Text
notificationDestinationConfig_uri = Lens.lens (\NotificationDestinationConfig' {uri} -> uri) (\s@NotificationDestinationConfig' {} a -> s {uri = a} :: NotificationDestinationConfig)

instance Core.FromJSON NotificationDestinationConfig where
  parseJSON =
    Core.withObject
      "NotificationDestinationConfig"
      ( \x ->
          NotificationDestinationConfig'
            Prelude.<$> (x Core..: "Uri")
      )

instance
  Prelude.Hashable
    NotificationDestinationConfig
  where
  hashWithSalt _salt NotificationDestinationConfig' {..} =
    _salt `Prelude.hashWithSalt` uri

instance Prelude.NFData NotificationDestinationConfig where
  rnf NotificationDestinationConfig' {..} =
    Prelude.rnf uri

instance Core.ToJSON NotificationDestinationConfig where
  toJSON NotificationDestinationConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Uri" Core..= uri)]
      )
