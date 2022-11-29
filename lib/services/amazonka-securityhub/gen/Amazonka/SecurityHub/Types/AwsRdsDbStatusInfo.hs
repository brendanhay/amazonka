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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbStatusInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbStatusInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the status of a read replica.
--
-- /See:/ 'newAwsRdsDbStatusInfo' smart constructor.
data AwsRdsDbStatusInfo = AwsRdsDbStatusInfo'
  { -- | If the read replica is currently in an error state, provides the error
    -- details.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status of the read replica instance.
    status :: Prelude.Maybe Prelude.Text,
    -- | Whether the read replica instance is operating normally.
    normal :: Prelude.Maybe Prelude.Bool,
    -- | The type of status. For a read replica, the status type is read
    -- replication.
    statusType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbStatusInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'awsRdsDbStatusInfo_message' - If the read replica is currently in an error state, provides the error
-- details.
--
-- 'status', 'awsRdsDbStatusInfo_status' - The status of the read replica instance.
--
-- 'normal', 'awsRdsDbStatusInfo_normal' - Whether the read replica instance is operating normally.
--
-- 'statusType', 'awsRdsDbStatusInfo_statusType' - The type of status. For a read replica, the status type is read
-- replication.
newAwsRdsDbStatusInfo ::
  AwsRdsDbStatusInfo
newAwsRdsDbStatusInfo =
  AwsRdsDbStatusInfo'
    { message = Prelude.Nothing,
      status = Prelude.Nothing,
      normal = Prelude.Nothing,
      statusType = Prelude.Nothing
    }

-- | If the read replica is currently in an error state, provides the error
-- details.
awsRdsDbStatusInfo_message :: Lens.Lens' AwsRdsDbStatusInfo (Prelude.Maybe Prelude.Text)
awsRdsDbStatusInfo_message = Lens.lens (\AwsRdsDbStatusInfo' {message} -> message) (\s@AwsRdsDbStatusInfo' {} a -> s {message = a} :: AwsRdsDbStatusInfo)

-- | The status of the read replica instance.
awsRdsDbStatusInfo_status :: Lens.Lens' AwsRdsDbStatusInfo (Prelude.Maybe Prelude.Text)
awsRdsDbStatusInfo_status = Lens.lens (\AwsRdsDbStatusInfo' {status} -> status) (\s@AwsRdsDbStatusInfo' {} a -> s {status = a} :: AwsRdsDbStatusInfo)

-- | Whether the read replica instance is operating normally.
awsRdsDbStatusInfo_normal :: Lens.Lens' AwsRdsDbStatusInfo (Prelude.Maybe Prelude.Bool)
awsRdsDbStatusInfo_normal = Lens.lens (\AwsRdsDbStatusInfo' {normal} -> normal) (\s@AwsRdsDbStatusInfo' {} a -> s {normal = a} :: AwsRdsDbStatusInfo)

-- | The type of status. For a read replica, the status type is read
-- replication.
awsRdsDbStatusInfo_statusType :: Lens.Lens' AwsRdsDbStatusInfo (Prelude.Maybe Prelude.Text)
awsRdsDbStatusInfo_statusType = Lens.lens (\AwsRdsDbStatusInfo' {statusType} -> statusType) (\s@AwsRdsDbStatusInfo' {} a -> s {statusType = a} :: AwsRdsDbStatusInfo)

instance Core.FromJSON AwsRdsDbStatusInfo where
  parseJSON =
    Core.withObject
      "AwsRdsDbStatusInfo"
      ( \x ->
          AwsRdsDbStatusInfo'
            Prelude.<$> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Normal")
            Prelude.<*> (x Core..:? "StatusType")
      )

instance Prelude.Hashable AwsRdsDbStatusInfo where
  hashWithSalt _salt AwsRdsDbStatusInfo' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` normal
      `Prelude.hashWithSalt` statusType

instance Prelude.NFData AwsRdsDbStatusInfo where
  rnf AwsRdsDbStatusInfo' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf normal
      `Prelude.seq` Prelude.rnf statusType

instance Core.ToJSON AwsRdsDbStatusInfo where
  toJSON AwsRdsDbStatusInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Message" Core..=) Prelude.<$> message,
            ("Status" Core..=) Prelude.<$> status,
            ("Normal" Core..=) Prelude.<$> normal,
            ("StatusType" Core..=) Prelude.<$> statusType
          ]
      )
