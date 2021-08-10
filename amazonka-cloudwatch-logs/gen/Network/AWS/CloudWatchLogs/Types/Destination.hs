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
-- Module      : Network.AWS.CloudWatchLogs.Types.Destination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.Destination where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a cross-account destination that receives subscription log
-- events.
--
-- /See:/ 'newDestination' smart constructor.
data Destination = Destination'
  { -- | The creation time of the destination, expressed as the number of
    -- milliseconds after Jan 1, 1970 00:00:00 UTC.
    creationTime :: Prelude.Maybe Prelude.Natural,
    -- | A role for impersonation, used when delivering log events to the target.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the destination.
    destinationName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of this destination.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the physical target where the log
    -- events are delivered (for example, a Kinesis stream).
    targetArn :: Prelude.Maybe Prelude.Text,
    -- | An IAM policy document that governs which AWS accounts can create
    -- subscription filters against this destination.
    accessPolicy :: Prelude.Maybe Prelude.Text
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
-- 'creationTime', 'destination_creationTime' - The creation time of the destination, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- 'roleArn', 'destination_roleArn' - A role for impersonation, used when delivering log events to the target.
--
-- 'destinationName', 'destination_destinationName' - The name of the destination.
--
-- 'arn', 'destination_arn' - The ARN of this destination.
--
-- 'targetArn', 'destination_targetArn' - The Amazon Resource Name (ARN) of the physical target where the log
-- events are delivered (for example, a Kinesis stream).
--
-- 'accessPolicy', 'destination_accessPolicy' - An IAM policy document that governs which AWS accounts can create
-- subscription filters against this destination.
newDestination ::
  Destination
newDestination =
  Destination'
    { creationTime = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      destinationName = Prelude.Nothing,
      arn = Prelude.Nothing,
      targetArn = Prelude.Nothing,
      accessPolicy = Prelude.Nothing
    }

-- | The creation time of the destination, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
destination_creationTime :: Lens.Lens' Destination (Prelude.Maybe Prelude.Natural)
destination_creationTime = Lens.lens (\Destination' {creationTime} -> creationTime) (\s@Destination' {} a -> s {creationTime = a} :: Destination)

-- | A role for impersonation, used when delivering log events to the target.
destination_roleArn :: Lens.Lens' Destination (Prelude.Maybe Prelude.Text)
destination_roleArn = Lens.lens (\Destination' {roleArn} -> roleArn) (\s@Destination' {} a -> s {roleArn = a} :: Destination)

-- | The name of the destination.
destination_destinationName :: Lens.Lens' Destination (Prelude.Maybe Prelude.Text)
destination_destinationName = Lens.lens (\Destination' {destinationName} -> destinationName) (\s@Destination' {} a -> s {destinationName = a} :: Destination)

-- | The ARN of this destination.
destination_arn :: Lens.Lens' Destination (Prelude.Maybe Prelude.Text)
destination_arn = Lens.lens (\Destination' {arn} -> arn) (\s@Destination' {} a -> s {arn = a} :: Destination)

-- | The Amazon Resource Name (ARN) of the physical target where the log
-- events are delivered (for example, a Kinesis stream).
destination_targetArn :: Lens.Lens' Destination (Prelude.Maybe Prelude.Text)
destination_targetArn = Lens.lens (\Destination' {targetArn} -> targetArn) (\s@Destination' {} a -> s {targetArn = a} :: Destination)

-- | An IAM policy document that governs which AWS accounts can create
-- subscription filters against this destination.
destination_accessPolicy :: Lens.Lens' Destination (Prelude.Maybe Prelude.Text)
destination_accessPolicy = Lens.lens (\Destination' {accessPolicy} -> accessPolicy) (\s@Destination' {} a -> s {accessPolicy = a} :: Destination)

instance Core.FromJSON Destination where
  parseJSON =
    Core.withObject
      "Destination"
      ( \x ->
          Destination'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "roleArn")
            Prelude.<*> (x Core..:? "destinationName")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "targetArn")
            Prelude.<*> (x Core..:? "accessPolicy")
      )

instance Prelude.Hashable Destination

instance Prelude.NFData Destination
