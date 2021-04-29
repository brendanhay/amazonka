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
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the Amazon Kinesis Firehose delivery stream that is configured
-- as the streaming source in the application input configuration.
--
-- /See:/ 'newKinesisFirehoseInputDescription' smart constructor.
data KinesisFirehoseInputDescription = KinesisFirehoseInputDescription'
  { -- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery
    -- stream.
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics assumes to access the
    -- stream.
    roleARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KinesisFirehoseInputDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'kinesisFirehoseInputDescription_resourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery
-- stream.
--
-- 'roleARN', 'kinesisFirehoseInputDescription_roleARN' - ARN of the IAM role that Amazon Kinesis Analytics assumes to access the
-- stream.
newKinesisFirehoseInputDescription ::
  KinesisFirehoseInputDescription
newKinesisFirehoseInputDescription =
  KinesisFirehoseInputDescription'
    { resourceARN =
        Prelude.Nothing,
      roleARN = Prelude.Nothing
    }

-- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery
-- stream.
kinesisFirehoseInputDescription_resourceARN :: Lens.Lens' KinesisFirehoseInputDescription (Prelude.Maybe Prelude.Text)
kinesisFirehoseInputDescription_resourceARN = Lens.lens (\KinesisFirehoseInputDescription' {resourceARN} -> resourceARN) (\s@KinesisFirehoseInputDescription' {} a -> s {resourceARN = a} :: KinesisFirehoseInputDescription)

-- | ARN of the IAM role that Amazon Kinesis Analytics assumes to access the
-- stream.
kinesisFirehoseInputDescription_roleARN :: Lens.Lens' KinesisFirehoseInputDescription (Prelude.Maybe Prelude.Text)
kinesisFirehoseInputDescription_roleARN = Lens.lens (\KinesisFirehoseInputDescription' {roleARN} -> roleARN) (\s@KinesisFirehoseInputDescription' {} a -> s {roleARN = a} :: KinesisFirehoseInputDescription)

instance
  Prelude.FromJSON
    KinesisFirehoseInputDescription
  where
  parseJSON =
    Prelude.withObject
      "KinesisFirehoseInputDescription"
      ( \x ->
          KinesisFirehoseInputDescription'
            Prelude.<$> (x Prelude..:? "ResourceARN")
            Prelude.<*> (x Prelude..:? "RoleARN")
      )

instance
  Prelude.Hashable
    KinesisFirehoseInputDescription

instance
  Prelude.NFData
    KinesisFirehoseInputDescription
