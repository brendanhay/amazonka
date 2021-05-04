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
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputDescription where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the Amazon Kinesis stream that is configured as the streaming
-- source in the application input configuration.
--
-- /See:/ 'newKinesisStreamsInputDescription' smart constructor.
data KinesisStreamsInputDescription = KinesisStreamsInputDescription'
  { -- | Amazon Resource Name (ARN) of the Amazon Kinesis stream.
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
    -- the stream.
    roleARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KinesisStreamsInputDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'kinesisStreamsInputDescription_resourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis stream.
--
-- 'roleARN', 'kinesisStreamsInputDescription_roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream.
newKinesisStreamsInputDescription ::
  KinesisStreamsInputDescription
newKinesisStreamsInputDescription =
  KinesisStreamsInputDescription'
    { resourceARN =
        Prelude.Nothing,
      roleARN = Prelude.Nothing
    }

-- | Amazon Resource Name (ARN) of the Amazon Kinesis stream.
kinesisStreamsInputDescription_resourceARN :: Lens.Lens' KinesisStreamsInputDescription (Prelude.Maybe Prelude.Text)
kinesisStreamsInputDescription_resourceARN = Lens.lens (\KinesisStreamsInputDescription' {resourceARN} -> resourceARN) (\s@KinesisStreamsInputDescription' {} a -> s {resourceARN = a} :: KinesisStreamsInputDescription)

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream.
kinesisStreamsInputDescription_roleARN :: Lens.Lens' KinesisStreamsInputDescription (Prelude.Maybe Prelude.Text)
kinesisStreamsInputDescription_roleARN = Lens.lens (\KinesisStreamsInputDescription' {roleARN} -> roleARN) (\s@KinesisStreamsInputDescription' {} a -> s {roleARN = a} :: KinesisStreamsInputDescription)

instance
  Prelude.FromJSON
    KinesisStreamsInputDescription
  where
  parseJSON =
    Prelude.withObject
      "KinesisStreamsInputDescription"
      ( \x ->
          KinesisStreamsInputDescription'
            Prelude.<$> (x Prelude..:? "ResourceARN")
            Prelude.<*> (x Prelude..:? "RoleARN")
      )

instance
  Prelude.Hashable
    KinesisStreamsInputDescription

instance
  Prelude.NFData
    KinesisStreamsInputDescription
