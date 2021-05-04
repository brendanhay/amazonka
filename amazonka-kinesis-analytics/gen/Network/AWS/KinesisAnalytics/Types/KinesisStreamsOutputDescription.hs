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
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputDescription where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | For an application output, describes the Amazon Kinesis stream
-- configured as its destination.
--
-- /See:/ 'newKinesisStreamsOutputDescription' smart constructor.
data KinesisStreamsOutputDescription = KinesisStreamsOutputDescription'
  { -- | Amazon Resource Name (ARN) of the Amazon Kinesis stream.
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
    -- the stream.
    roleARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KinesisStreamsOutputDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'kinesisStreamsOutputDescription_resourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis stream.
--
-- 'roleARN', 'kinesisStreamsOutputDescription_roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream.
newKinesisStreamsOutputDescription ::
  KinesisStreamsOutputDescription
newKinesisStreamsOutputDescription =
  KinesisStreamsOutputDescription'
    { resourceARN =
        Prelude.Nothing,
      roleARN = Prelude.Nothing
    }

-- | Amazon Resource Name (ARN) of the Amazon Kinesis stream.
kinesisStreamsOutputDescription_resourceARN :: Lens.Lens' KinesisStreamsOutputDescription (Prelude.Maybe Prelude.Text)
kinesisStreamsOutputDescription_resourceARN = Lens.lens (\KinesisStreamsOutputDescription' {resourceARN} -> resourceARN) (\s@KinesisStreamsOutputDescription' {} a -> s {resourceARN = a} :: KinesisStreamsOutputDescription)

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream.
kinesisStreamsOutputDescription_roleARN :: Lens.Lens' KinesisStreamsOutputDescription (Prelude.Maybe Prelude.Text)
kinesisStreamsOutputDescription_roleARN = Lens.lens (\KinesisStreamsOutputDescription' {roleARN} -> roleARN) (\s@KinesisStreamsOutputDescription' {} a -> s {roleARN = a} :: KinesisStreamsOutputDescription)

instance
  Prelude.FromJSON
    KinesisStreamsOutputDescription
  where
  parseJSON =
    Prelude.withObject
      "KinesisStreamsOutputDescription"
      ( \x ->
          KinesisStreamsOutputDescription'
            Prelude.<$> (x Prelude..:? "ResourceARN")
            Prelude.<*> (x Prelude..:? "RoleARN")
      )

instance
  Prelude.Hashable
    KinesisStreamsOutputDescription

instance
  Prelude.NFData
    KinesisStreamsOutputDescription
