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
-- Module      : Amazonka.SecurityHub.Types.AwsAmazonMqBrokerUsersDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAmazonMqBrokerUsersDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details about the broker usernames for the specified broker.
-- Doesn\'t apply to RabbitMQ brokers.
--
-- /See:/ 'newAwsAmazonMqBrokerUsersDetails' smart constructor.
data AwsAmazonMqBrokerUsersDetails = AwsAmazonMqBrokerUsersDetails'
  { -- | The type of change pending for the broker user.
    pendingChange :: Prelude.Maybe Prelude.Text,
    -- | The username of the broker user.
    username :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAmazonMqBrokerUsersDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pendingChange', 'awsAmazonMqBrokerUsersDetails_pendingChange' - The type of change pending for the broker user.
--
-- 'username', 'awsAmazonMqBrokerUsersDetails_username' - The username of the broker user.
newAwsAmazonMqBrokerUsersDetails ::
  AwsAmazonMqBrokerUsersDetails
newAwsAmazonMqBrokerUsersDetails =
  AwsAmazonMqBrokerUsersDetails'
    { pendingChange =
        Prelude.Nothing,
      username = Prelude.Nothing
    }

-- | The type of change pending for the broker user.
awsAmazonMqBrokerUsersDetails_pendingChange :: Lens.Lens' AwsAmazonMqBrokerUsersDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerUsersDetails_pendingChange = Lens.lens (\AwsAmazonMqBrokerUsersDetails' {pendingChange} -> pendingChange) (\s@AwsAmazonMqBrokerUsersDetails' {} a -> s {pendingChange = a} :: AwsAmazonMqBrokerUsersDetails)

-- | The username of the broker user.
awsAmazonMqBrokerUsersDetails_username :: Lens.Lens' AwsAmazonMqBrokerUsersDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerUsersDetails_username = Lens.lens (\AwsAmazonMqBrokerUsersDetails' {username} -> username) (\s@AwsAmazonMqBrokerUsersDetails' {} a -> s {username = a} :: AwsAmazonMqBrokerUsersDetails)

instance Data.FromJSON AwsAmazonMqBrokerUsersDetails where
  parseJSON =
    Data.withObject
      "AwsAmazonMqBrokerUsersDetails"
      ( \x ->
          AwsAmazonMqBrokerUsersDetails'
            Prelude.<$> (x Data..:? "PendingChange")
            Prelude.<*> (x Data..:? "Username")
      )

instance
  Prelude.Hashable
    AwsAmazonMqBrokerUsersDetails
  where
  hashWithSalt _salt AwsAmazonMqBrokerUsersDetails' {..} =
    _salt
      `Prelude.hashWithSalt` pendingChange
      `Prelude.hashWithSalt` username

instance Prelude.NFData AwsAmazonMqBrokerUsersDetails where
  rnf AwsAmazonMqBrokerUsersDetails' {..} =
    Prelude.rnf pendingChange
      `Prelude.seq` Prelude.rnf username

instance Data.ToJSON AwsAmazonMqBrokerUsersDetails where
  toJSON AwsAmazonMqBrokerUsersDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PendingChange" Data..=) Prelude.<$> pendingChange,
            ("Username" Data..=) Prelude.<$> username
          ]
      )
