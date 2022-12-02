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
-- Module      : Amazonka.GuardDuty.Types.DestinationProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.DestinationProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the Amazon Resource Name (ARN) of the resource to publish to,
-- such as an S3 bucket, and the ARN of the KMS key to use to encrypt
-- published findings.
--
-- /See:/ 'newDestinationProperties' smart constructor.
data DestinationProperties = DestinationProperties'
  { -- | The ARN of the KMS key to use for encryption.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the resource to publish to.
    --
    -- To specify an S3 bucket folder use the following format:
    -- @arn:aws:s3:::DOC-EXAMPLE-BUCKET\/myFolder\/@
    destinationArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyArn', 'destinationProperties_kmsKeyArn' - The ARN of the KMS key to use for encryption.
--
-- 'destinationArn', 'destinationProperties_destinationArn' - The ARN of the resource to publish to.
--
-- To specify an S3 bucket folder use the following format:
-- @arn:aws:s3:::DOC-EXAMPLE-BUCKET\/myFolder\/@
newDestinationProperties ::
  DestinationProperties
newDestinationProperties =
  DestinationProperties'
    { kmsKeyArn = Prelude.Nothing,
      destinationArn = Prelude.Nothing
    }

-- | The ARN of the KMS key to use for encryption.
destinationProperties_kmsKeyArn :: Lens.Lens' DestinationProperties (Prelude.Maybe Prelude.Text)
destinationProperties_kmsKeyArn = Lens.lens (\DestinationProperties' {kmsKeyArn} -> kmsKeyArn) (\s@DestinationProperties' {} a -> s {kmsKeyArn = a} :: DestinationProperties)

-- | The ARN of the resource to publish to.
--
-- To specify an S3 bucket folder use the following format:
-- @arn:aws:s3:::DOC-EXAMPLE-BUCKET\/myFolder\/@
destinationProperties_destinationArn :: Lens.Lens' DestinationProperties (Prelude.Maybe Prelude.Text)
destinationProperties_destinationArn = Lens.lens (\DestinationProperties' {destinationArn} -> destinationArn) (\s@DestinationProperties' {} a -> s {destinationArn = a} :: DestinationProperties)

instance Data.FromJSON DestinationProperties where
  parseJSON =
    Data.withObject
      "DestinationProperties"
      ( \x ->
          DestinationProperties'
            Prelude.<$> (x Data..:? "kmsKeyArn")
            Prelude.<*> (x Data..:? "destinationArn")
      )

instance Prelude.Hashable DestinationProperties where
  hashWithSalt _salt DestinationProperties' {..} =
    _salt `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` destinationArn

instance Prelude.NFData DestinationProperties where
  rnf DestinationProperties' {..} =
    Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf destinationArn

instance Data.ToJSON DestinationProperties where
  toJSON DestinationProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("kmsKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            ("destinationArn" Data..=)
              Prelude.<$> destinationArn
          ]
      )
