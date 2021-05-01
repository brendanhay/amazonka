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
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputDescription where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | For an application output, describes the Amazon Kinesis Firehose
-- delivery stream configured as its destination.
--
-- /See:/ 'newKinesisFirehoseOutputDescription' smart constructor.
data KinesisFirehoseOutputDescription = KinesisFirehoseOutputDescription'
  { -- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery
    -- stream.
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
    -- the stream.
    roleARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KinesisFirehoseOutputDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'kinesisFirehoseOutputDescription_resourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery
-- stream.
--
-- 'roleARN', 'kinesisFirehoseOutputDescription_roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream.
newKinesisFirehoseOutputDescription ::
  KinesisFirehoseOutputDescription
newKinesisFirehoseOutputDescription =
  KinesisFirehoseOutputDescription'
    { resourceARN =
        Prelude.Nothing,
      roleARN = Prelude.Nothing
    }

-- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery
-- stream.
kinesisFirehoseOutputDescription_resourceARN :: Lens.Lens' KinesisFirehoseOutputDescription (Prelude.Maybe Prelude.Text)
kinesisFirehoseOutputDescription_resourceARN = Lens.lens (\KinesisFirehoseOutputDescription' {resourceARN} -> resourceARN) (\s@KinesisFirehoseOutputDescription' {} a -> s {resourceARN = a} :: KinesisFirehoseOutputDescription)

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream.
kinesisFirehoseOutputDescription_roleARN :: Lens.Lens' KinesisFirehoseOutputDescription (Prelude.Maybe Prelude.Text)
kinesisFirehoseOutputDescription_roleARN = Lens.lens (\KinesisFirehoseOutputDescription' {roleARN} -> roleARN) (\s@KinesisFirehoseOutputDescription' {} a -> s {roleARN = a} :: KinesisFirehoseOutputDescription)

instance
  Prelude.FromJSON
    KinesisFirehoseOutputDescription
  where
  parseJSON =
    Prelude.withObject
      "KinesisFirehoseOutputDescription"
      ( \x ->
          KinesisFirehoseOutputDescription'
            Prelude.<$> (x Prelude..:? "ResourceARN")
            Prelude.<*> (x Prelude..:? "RoleARN")
      )

instance
  Prelude.Hashable
    KinesisFirehoseOutputDescription

instance
  Prelude.NFData
    KinesisFirehoseOutputDescription
