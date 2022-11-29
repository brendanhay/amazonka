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
-- Module      : Amazonka.KinesisAnalytics.Types.KinesisStreamsOutputDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.KinesisStreamsOutputDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | For an application output, describes the Amazon Kinesis stream
-- configured as its destination.
--
-- /See:/ 'newKinesisStreamsOutputDescription' smart constructor.
data KinesisStreamsOutputDescription = KinesisStreamsOutputDescription'
  { -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
    -- the stream.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the Amazon Kinesis stream.
    resourceARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisStreamsOutputDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'kinesisStreamsOutputDescription_roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream.
--
-- 'resourceARN', 'kinesisStreamsOutputDescription_resourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis stream.
newKinesisStreamsOutputDescription ::
  KinesisStreamsOutputDescription
newKinesisStreamsOutputDescription =
  KinesisStreamsOutputDescription'
    { roleARN =
        Prelude.Nothing,
      resourceARN = Prelude.Nothing
    }

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream.
kinesisStreamsOutputDescription_roleARN :: Lens.Lens' KinesisStreamsOutputDescription (Prelude.Maybe Prelude.Text)
kinesisStreamsOutputDescription_roleARN = Lens.lens (\KinesisStreamsOutputDescription' {roleARN} -> roleARN) (\s@KinesisStreamsOutputDescription' {} a -> s {roleARN = a} :: KinesisStreamsOutputDescription)

-- | Amazon Resource Name (ARN) of the Amazon Kinesis stream.
kinesisStreamsOutputDescription_resourceARN :: Lens.Lens' KinesisStreamsOutputDescription (Prelude.Maybe Prelude.Text)
kinesisStreamsOutputDescription_resourceARN = Lens.lens (\KinesisStreamsOutputDescription' {resourceARN} -> resourceARN) (\s@KinesisStreamsOutputDescription' {} a -> s {resourceARN = a} :: KinesisStreamsOutputDescription)

instance
  Core.FromJSON
    KinesisStreamsOutputDescription
  where
  parseJSON =
    Core.withObject
      "KinesisStreamsOutputDescription"
      ( \x ->
          KinesisStreamsOutputDescription'
            Prelude.<$> (x Core..:? "RoleARN")
            Prelude.<*> (x Core..:? "ResourceARN")
      )

instance
  Prelude.Hashable
    KinesisStreamsOutputDescription
  where
  hashWithSalt
    _salt
    KinesisStreamsOutputDescription' {..} =
      _salt `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` resourceARN

instance
  Prelude.NFData
    KinesisStreamsOutputDescription
  where
  rnf KinesisStreamsOutputDescription' {..} =
    Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf resourceARN
