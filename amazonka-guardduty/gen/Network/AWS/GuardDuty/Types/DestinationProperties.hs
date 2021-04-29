{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the Amazon Resource Name (ARN) of the resource to publish to,
-- such as an S3 bucket, and the ARN of the KMS key to use to encrypt
-- published findings.
--
-- /See:/ 'newDestinationProperties' smart constructor.
data DestinationProperties = DestinationProperties'
  { -- | The ARN of the resource to publish to.
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the KMS key to use for encryption.
    kmsKeyArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing
    }

-- | The ARN of the resource to publish to.
destinationProperties_destinationArn :: Lens.Lens' DestinationProperties (Prelude.Maybe Prelude.Text)
destinationProperties_destinationArn = Lens.lens (\DestinationProperties' {destinationArn} -> destinationArn) (\s@DestinationProperties' {} a -> s {destinationArn = a} :: DestinationProperties)

-- | The ARN of the KMS key to use for encryption.
destinationProperties_kmsKeyArn :: Lens.Lens' DestinationProperties (Prelude.Maybe Prelude.Text)
destinationProperties_kmsKeyArn = Lens.lens (\DestinationProperties' {kmsKeyArn} -> kmsKeyArn) (\s@DestinationProperties' {} a -> s {kmsKeyArn = a} :: DestinationProperties)

instance Prelude.FromJSON DestinationProperties where
  parseJSON =
    Prelude.withObject
      "DestinationProperties"
      ( \x ->
          DestinationProperties'
            Prelude.<$> (x Prelude..:? "destinationArn")
            Prelude.<*> (x Prelude..:? "kmsKeyArn")
      )

instance Prelude.Hashable DestinationProperties

instance Prelude.NFData DestinationProperties

instance Prelude.ToJSON DestinationProperties where
  toJSON DestinationProperties' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("destinationArn" Prelude..=)
              Prelude.<$> destinationArn,
            ("kmsKeyArn" Prelude..=) Prelude.<$> kmsKeyArn
          ]
      )
