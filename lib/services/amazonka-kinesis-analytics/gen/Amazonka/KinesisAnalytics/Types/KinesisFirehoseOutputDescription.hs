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
-- Module      : Amazonka.KinesisAnalytics.Types.KinesisFirehoseOutputDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.KinesisFirehoseOutputDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For an application output, describes the Amazon Kinesis Firehose
-- delivery stream configured as its destination.
--
-- /See:/ 'newKinesisFirehoseOutputDescription' smart constructor.
data KinesisFirehoseOutputDescription = KinesisFirehoseOutputDescription'
  { -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
    -- the stream.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery
    -- stream.
    resourceARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisFirehoseOutputDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'kinesisFirehoseOutputDescription_roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream.
--
-- 'resourceARN', 'kinesisFirehoseOutputDescription_resourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery
-- stream.
newKinesisFirehoseOutputDescription ::
  KinesisFirehoseOutputDescription
newKinesisFirehoseOutputDescription =
  KinesisFirehoseOutputDescription'
    { roleARN =
        Prelude.Nothing,
      resourceARN = Prelude.Nothing
    }

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream.
kinesisFirehoseOutputDescription_roleARN :: Lens.Lens' KinesisFirehoseOutputDescription (Prelude.Maybe Prelude.Text)
kinesisFirehoseOutputDescription_roleARN = Lens.lens (\KinesisFirehoseOutputDescription' {roleARN} -> roleARN) (\s@KinesisFirehoseOutputDescription' {} a -> s {roleARN = a} :: KinesisFirehoseOutputDescription)

-- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery
-- stream.
kinesisFirehoseOutputDescription_resourceARN :: Lens.Lens' KinesisFirehoseOutputDescription (Prelude.Maybe Prelude.Text)
kinesisFirehoseOutputDescription_resourceARN = Lens.lens (\KinesisFirehoseOutputDescription' {resourceARN} -> resourceARN) (\s@KinesisFirehoseOutputDescription' {} a -> s {resourceARN = a} :: KinesisFirehoseOutputDescription)

instance
  Data.FromJSON
    KinesisFirehoseOutputDescription
  where
  parseJSON =
    Data.withObject
      "KinesisFirehoseOutputDescription"
      ( \x ->
          KinesisFirehoseOutputDescription'
            Prelude.<$> (x Data..:? "RoleARN")
            Prelude.<*> (x Data..:? "ResourceARN")
      )

instance
  Prelude.Hashable
    KinesisFirehoseOutputDescription
  where
  hashWithSalt
    _salt
    KinesisFirehoseOutputDescription' {..} =
      _salt `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` resourceARN

instance
  Prelude.NFData
    KinesisFirehoseOutputDescription
  where
  rnf KinesisFirehoseOutputDescription' {..} =
    Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf resourceARN
