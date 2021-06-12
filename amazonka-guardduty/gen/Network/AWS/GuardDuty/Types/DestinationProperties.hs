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
-- Module      : Network.AWS.GuardDuty.Types.DestinationProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DestinationProperties where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the Amazon Resource Name (ARN) of the resource to publish to,
-- such as an S3 bucket, and the ARN of the KMS key to use to encrypt
-- published findings.
--
-- /See:/ 'newDestinationProperties' smart constructor.
data DestinationProperties = DestinationProperties'
  { -- | The ARN of the resource to publish to.
    destinationArn :: Core.Maybe Core.Text,
    -- | The ARN of the KMS key to use for encryption.
    kmsKeyArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DestinationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationArn', 'destinationProperties_destinationArn' - The ARN of the resource to publish to.
--
-- 'kmsKeyArn', 'destinationProperties_kmsKeyArn' - The ARN of the KMS key to use for encryption.
newDestinationProperties ::
  DestinationProperties
newDestinationProperties =
  DestinationProperties'
    { destinationArn =
        Core.Nothing,
      kmsKeyArn = Core.Nothing
    }

-- | The ARN of the resource to publish to.
destinationProperties_destinationArn :: Lens.Lens' DestinationProperties (Core.Maybe Core.Text)
destinationProperties_destinationArn = Lens.lens (\DestinationProperties' {destinationArn} -> destinationArn) (\s@DestinationProperties' {} a -> s {destinationArn = a} :: DestinationProperties)

-- | The ARN of the KMS key to use for encryption.
destinationProperties_kmsKeyArn :: Lens.Lens' DestinationProperties (Core.Maybe Core.Text)
destinationProperties_kmsKeyArn = Lens.lens (\DestinationProperties' {kmsKeyArn} -> kmsKeyArn) (\s@DestinationProperties' {} a -> s {kmsKeyArn = a} :: DestinationProperties)

instance Core.FromJSON DestinationProperties where
  parseJSON =
    Core.withObject
      "DestinationProperties"
      ( \x ->
          DestinationProperties'
            Core.<$> (x Core..:? "destinationArn")
            Core.<*> (x Core..:? "kmsKeyArn")
      )

instance Core.Hashable DestinationProperties

instance Core.NFData DestinationProperties

instance Core.ToJSON DestinationProperties where
  toJSON DestinationProperties' {..} =
    Core.object
      ( Core.catMaybes
          [ ("destinationArn" Core..=) Core.<$> destinationArn,
            ("kmsKeyArn" Core..=) Core.<$> kmsKeyArn
          ]
      )
