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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.KinesisFirehoseOutputDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.KinesisFirehoseOutputDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | For a SQL-based Kinesis Data Analytics application\'s output, describes
-- the Kinesis Data Firehose delivery stream that is configured as its
-- destination.
--
-- /See:/ 'newKinesisFirehoseOutputDescription' smart constructor.
data KinesisFirehoseOutputDescription = KinesisFirehoseOutputDescription'
  { -- | The ARN of the IAM role that Kinesis Data Analytics can assume to access
    -- the stream.
    --
    -- Provided for backward compatibility. Applications that are created with
    -- the current API version have an application-level service execution role
    -- rather than a resource-level role.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the delivery stream.
    resourceARN :: Prelude.Text
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
-- 'roleARN', 'kinesisFirehoseOutputDescription_roleARN' - The ARN of the IAM role that Kinesis Data Analytics can assume to access
-- the stream.
--
-- Provided for backward compatibility. Applications that are created with
-- the current API version have an application-level service execution role
-- rather than a resource-level role.
--
-- 'resourceARN', 'kinesisFirehoseOutputDescription_resourceARN' - The Amazon Resource Name (ARN) of the delivery stream.
newKinesisFirehoseOutputDescription ::
  -- | 'resourceARN'
  Prelude.Text ->
  KinesisFirehoseOutputDescription
newKinesisFirehoseOutputDescription pResourceARN_ =
  KinesisFirehoseOutputDescription'
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
kinesisFirehoseOutputDescription_roleARN :: Lens.Lens' KinesisFirehoseOutputDescription (Prelude.Maybe Prelude.Text)
kinesisFirehoseOutputDescription_roleARN = Lens.lens (\KinesisFirehoseOutputDescription' {roleARN} -> roleARN) (\s@KinesisFirehoseOutputDescription' {} a -> s {roleARN = a} :: KinesisFirehoseOutputDescription)

-- | The Amazon Resource Name (ARN) of the delivery stream.
kinesisFirehoseOutputDescription_resourceARN :: Lens.Lens' KinesisFirehoseOutputDescription Prelude.Text
kinesisFirehoseOutputDescription_resourceARN = Lens.lens (\KinesisFirehoseOutputDescription' {resourceARN} -> resourceARN) (\s@KinesisFirehoseOutputDescription' {} a -> s {resourceARN = a} :: KinesisFirehoseOutputDescription)

instance
  Core.FromJSON
    KinesisFirehoseOutputDescription
  where
  parseJSON =
    Core.withObject
      "KinesisFirehoseOutputDescription"
      ( \x ->
          KinesisFirehoseOutputDescription'
            Prelude.<$> (x Core..:? "RoleARN")
            Prelude.<*> (x Core..: "ResourceARN")
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
