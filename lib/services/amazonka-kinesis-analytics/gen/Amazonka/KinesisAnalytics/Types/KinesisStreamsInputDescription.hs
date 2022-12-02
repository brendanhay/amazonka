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
-- Module      : Amazonka.KinesisAnalytics.Types.KinesisStreamsInputDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.KinesisStreamsInputDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the Amazon Kinesis stream that is configured as the streaming
-- source in the application input configuration.
--
-- /See:/ 'newKinesisStreamsInputDescription' smart constructor.
data KinesisStreamsInputDescription = KinesisStreamsInputDescription'
  { -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
    -- the stream.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the Amazon Kinesis stream.
    resourceARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisStreamsInputDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'kinesisStreamsInputDescription_roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream.
--
-- 'resourceARN', 'kinesisStreamsInputDescription_resourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis stream.
newKinesisStreamsInputDescription ::
  KinesisStreamsInputDescription
newKinesisStreamsInputDescription =
  KinesisStreamsInputDescription'
    { roleARN =
        Prelude.Nothing,
      resourceARN = Prelude.Nothing
    }

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream.
kinesisStreamsInputDescription_roleARN :: Lens.Lens' KinesisStreamsInputDescription (Prelude.Maybe Prelude.Text)
kinesisStreamsInputDescription_roleARN = Lens.lens (\KinesisStreamsInputDescription' {roleARN} -> roleARN) (\s@KinesisStreamsInputDescription' {} a -> s {roleARN = a} :: KinesisStreamsInputDescription)

-- | Amazon Resource Name (ARN) of the Amazon Kinesis stream.
kinesisStreamsInputDescription_resourceARN :: Lens.Lens' KinesisStreamsInputDescription (Prelude.Maybe Prelude.Text)
kinesisStreamsInputDescription_resourceARN = Lens.lens (\KinesisStreamsInputDescription' {resourceARN} -> resourceARN) (\s@KinesisStreamsInputDescription' {} a -> s {resourceARN = a} :: KinesisStreamsInputDescription)

instance Data.FromJSON KinesisStreamsInputDescription where
  parseJSON =
    Data.withObject
      "KinesisStreamsInputDescription"
      ( \x ->
          KinesisStreamsInputDescription'
            Prelude.<$> (x Data..:? "RoleARN")
            Prelude.<*> (x Data..:? "ResourceARN")
      )

instance
  Prelude.Hashable
    KinesisStreamsInputDescription
  where
  hashWithSalt
    _salt
    KinesisStreamsInputDescription' {..} =
      _salt `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` resourceARN

instance
  Prelude.NFData
    KinesisStreamsInputDescription
  where
  rnf KinesisStreamsInputDescription' {..} =
    Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf resourceARN
