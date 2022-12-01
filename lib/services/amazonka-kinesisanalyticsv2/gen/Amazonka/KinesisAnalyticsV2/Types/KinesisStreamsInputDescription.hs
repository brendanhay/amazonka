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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.KinesisStreamsInputDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.KinesisStreamsInputDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | For a SQL-based Kinesis Data Analytics application, describes the
-- Kinesis data stream that is configured as the streaming source in the
-- application input configuration.
--
-- /See:/ 'newKinesisStreamsInputDescription' smart constructor.
data KinesisStreamsInputDescription = KinesisStreamsInputDescription'
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
-- Create a value of 'KinesisStreamsInputDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'kinesisStreamsInputDescription_roleARN' - The ARN of the IAM role that Kinesis Data Analytics can assume to access
-- the stream.
--
-- Provided for backward compatibility. Applications that are created with
-- the current API version have an application-level service execution role
-- rather than a resource-level role.
--
-- 'resourceARN', 'kinesisStreamsInputDescription_resourceARN' - The Amazon Resource Name (ARN) of the Kinesis data stream.
newKinesisStreamsInputDescription ::
  -- | 'resourceARN'
  Prelude.Text ->
  KinesisStreamsInputDescription
newKinesisStreamsInputDescription pResourceARN_ =
  KinesisStreamsInputDescription'
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
kinesisStreamsInputDescription_roleARN :: Lens.Lens' KinesisStreamsInputDescription (Prelude.Maybe Prelude.Text)
kinesisStreamsInputDescription_roleARN = Lens.lens (\KinesisStreamsInputDescription' {roleARN} -> roleARN) (\s@KinesisStreamsInputDescription' {} a -> s {roleARN = a} :: KinesisStreamsInputDescription)

-- | The Amazon Resource Name (ARN) of the Kinesis data stream.
kinesisStreamsInputDescription_resourceARN :: Lens.Lens' KinesisStreamsInputDescription Prelude.Text
kinesisStreamsInputDescription_resourceARN = Lens.lens (\KinesisStreamsInputDescription' {resourceARN} -> resourceARN) (\s@KinesisStreamsInputDescription' {} a -> s {resourceARN = a} :: KinesisStreamsInputDescription)

instance Core.FromJSON KinesisStreamsInputDescription where
  parseJSON =
    Core.withObject
      "KinesisStreamsInputDescription"
      ( \x ->
          KinesisStreamsInputDescription'
            Prelude.<$> (x Core..:? "RoleARN")
            Prelude.<*> (x Core..: "ResourceARN")
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
