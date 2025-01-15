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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.KinesisStreamsOutputDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.KinesisStreamsOutputDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For an SQL-based Kinesis Data Analytics application\'s output, describes
-- the Kinesis data stream that is configured as its destination.
--
-- /See:/ 'newKinesisStreamsOutputDescription' smart constructor.
data KinesisStreamsOutputDescription = KinesisStreamsOutputDescription'
  { -- | The ARN of the IAM role that Kinesis Data Analytics can assume to access
    -- the stream.
    --
    -- Provided for backward compatibility. Applications that are created with
    -- the current API version have an application-level service execution role
    -- rather than a resource-level role.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Kinesis data stream.
    resourceARN :: Prelude.Text
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
-- 'roleARN', 'kinesisStreamsOutputDescription_roleARN' - The ARN of the IAM role that Kinesis Data Analytics can assume to access
-- the stream.
--
-- Provided for backward compatibility. Applications that are created with
-- the current API version have an application-level service execution role
-- rather than a resource-level role.
--
-- 'resourceARN', 'kinesisStreamsOutputDescription_resourceARN' - The Amazon Resource Name (ARN) of the Kinesis data stream.
newKinesisStreamsOutputDescription ::
  -- | 'resourceARN'
  Prelude.Text ->
  KinesisStreamsOutputDescription
newKinesisStreamsOutputDescription pResourceARN_ =
  KinesisStreamsOutputDescription'
    { roleARN =
        Prelude.Nothing,
      resourceARN = pResourceARN_
    }

-- | The ARN of the IAM role that Kinesis Data Analytics can assume to access
-- the stream.
--
-- Provided for backward compatibility. Applications that are created with
-- the current API version have an application-level service execution role
-- rather than a resource-level role.
kinesisStreamsOutputDescription_roleARN :: Lens.Lens' KinesisStreamsOutputDescription (Prelude.Maybe Prelude.Text)
kinesisStreamsOutputDescription_roleARN = Lens.lens (\KinesisStreamsOutputDescription' {roleARN} -> roleARN) (\s@KinesisStreamsOutputDescription' {} a -> s {roleARN = a} :: KinesisStreamsOutputDescription)

-- | The Amazon Resource Name (ARN) of the Kinesis data stream.
kinesisStreamsOutputDescription_resourceARN :: Lens.Lens' KinesisStreamsOutputDescription Prelude.Text
kinesisStreamsOutputDescription_resourceARN = Lens.lens (\KinesisStreamsOutputDescription' {resourceARN} -> resourceARN) (\s@KinesisStreamsOutputDescription' {} a -> s {resourceARN = a} :: KinesisStreamsOutputDescription)

instance
  Data.FromJSON
    KinesisStreamsOutputDescription
  where
  parseJSON =
    Data.withObject
      "KinesisStreamsOutputDescription"
      ( \x ->
          KinesisStreamsOutputDescription'
            Prelude.<$> (x Data..:? "RoleARN")
            Prelude.<*> (x Data..: "ResourceARN")
      )

instance
  Prelude.Hashable
    KinesisStreamsOutputDescription
  where
  hashWithSalt
    _salt
    KinesisStreamsOutputDescription' {..} =
      _salt
        `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` resourceARN

instance
  Prelude.NFData
    KinesisStreamsOutputDescription
  where
  rnf KinesisStreamsOutputDescription' {..} =
    Prelude.rnf roleARN `Prelude.seq`
      Prelude.rnf resourceARN
