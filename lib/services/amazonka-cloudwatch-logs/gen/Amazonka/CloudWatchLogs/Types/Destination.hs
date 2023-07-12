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
-- Module      : Amazonka.CloudWatchLogs.Types.Destination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.Destination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a cross-account destination that receives subscription log
-- events.
--
-- /See:/ 'newDestination' smart constructor.
data Destination = Destination'
  { -- | An IAM policy document that governs which Amazon Web Services accounts
    -- can create subscription filters against this destination.
    accessPolicy :: Prelude.Maybe Prelude.Text,
    -- | The ARN of this destination.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The creation time of the destination, expressed as the number of
    -- milliseconds after Jan 1, 1970 00:00:00 UTC.
    creationTime :: Prelude.Maybe Prelude.Natural,
    -- | The name of the destination.
    destinationName :: Prelude.Maybe Prelude.Text,
    -- | A role for impersonation, used when delivering log events to the target.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the physical target where the log
    -- events are delivered (for example, a Kinesis stream).
    targetArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessPolicy', 'destination_accessPolicy' - An IAM policy document that governs which Amazon Web Services accounts
-- can create subscription filters against this destination.
--
-- 'arn', 'destination_arn' - The ARN of this destination.
--
-- 'creationTime', 'destination_creationTime' - The creation time of the destination, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- 'destinationName', 'destination_destinationName' - The name of the destination.
--
-- 'roleArn', 'destination_roleArn' - A role for impersonation, used when delivering log events to the target.
--
-- 'targetArn', 'destination_targetArn' - The Amazon Resource Name (ARN) of the physical target where the log
-- events are delivered (for example, a Kinesis stream).
newDestination ::
  Destination
newDestination =
  Destination'
    { accessPolicy = Prelude.Nothing,
      arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      destinationName = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      targetArn = Prelude.Nothing
    }

-- | An IAM policy document that governs which Amazon Web Services accounts
-- can create subscription filters against this destination.
destination_accessPolicy :: Lens.Lens' Destination (Prelude.Maybe Prelude.Text)
destination_accessPolicy = Lens.lens (\Destination' {accessPolicy} -> accessPolicy) (\s@Destination' {} a -> s {accessPolicy = a} :: Destination)

-- | The ARN of this destination.
destination_arn :: Lens.Lens' Destination (Prelude.Maybe Prelude.Text)
destination_arn = Lens.lens (\Destination' {arn} -> arn) (\s@Destination' {} a -> s {arn = a} :: Destination)

-- | The creation time of the destination, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
destination_creationTime :: Lens.Lens' Destination (Prelude.Maybe Prelude.Natural)
destination_creationTime = Lens.lens (\Destination' {creationTime} -> creationTime) (\s@Destination' {} a -> s {creationTime = a} :: Destination)

-- | The name of the destination.
destination_destinationName :: Lens.Lens' Destination (Prelude.Maybe Prelude.Text)
destination_destinationName = Lens.lens (\Destination' {destinationName} -> destinationName) (\s@Destination' {} a -> s {destinationName = a} :: Destination)

-- | A role for impersonation, used when delivering log events to the target.
destination_roleArn :: Lens.Lens' Destination (Prelude.Maybe Prelude.Text)
destination_roleArn = Lens.lens (\Destination' {roleArn} -> roleArn) (\s@Destination' {} a -> s {roleArn = a} :: Destination)

-- | The Amazon Resource Name (ARN) of the physical target where the log
-- events are delivered (for example, a Kinesis stream).
destination_targetArn :: Lens.Lens' Destination (Prelude.Maybe Prelude.Text)
destination_targetArn = Lens.lens (\Destination' {targetArn} -> targetArn) (\s@Destination' {} a -> s {targetArn = a} :: Destination)

instance Data.FromJSON Destination where
  parseJSON =
    Data.withObject
      "Destination"
      ( \x ->
          Destination'
            Prelude.<$> (x Data..:? "accessPolicy")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "destinationName")
            Prelude.<*> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "targetArn")
      )

instance Prelude.Hashable Destination where
  hashWithSalt _salt Destination' {..} =
    _salt
      `Prelude.hashWithSalt` accessPolicy
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` destinationName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` targetArn

instance Prelude.NFData Destination where
  rnf Destination' {..} =
    Prelude.rnf accessPolicy
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf destinationName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf targetArn
