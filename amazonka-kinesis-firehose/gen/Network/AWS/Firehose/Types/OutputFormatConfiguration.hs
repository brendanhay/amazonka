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
-- Module      : Network.AWS.Firehose.Types.OutputFormatConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.OutputFormatConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types.Serializer
import qualified Network.AWS.Lens as Lens

-- | Specifies the serializer that you want Kinesis Data Firehose to use to
-- convert the format of your data before it writes it to Amazon S3. This
-- parameter is required if @Enabled@ is set to true.
--
-- /See:/ 'newOutputFormatConfiguration' smart constructor.
data OutputFormatConfiguration = OutputFormatConfiguration'
  { -- | Specifies which serializer to use. You can choose either the ORC SerDe
    -- or the Parquet SerDe. If both are non-null, the server rejects the
    -- request.
    serializer :: Core.Maybe Serializer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OutputFormatConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serializer', 'outputFormatConfiguration_serializer' - Specifies which serializer to use. You can choose either the ORC SerDe
-- or the Parquet SerDe. If both are non-null, the server rejects the
-- request.
newOutputFormatConfiguration ::
  OutputFormatConfiguration
newOutputFormatConfiguration =
  OutputFormatConfiguration'
    { serializer =
        Core.Nothing
    }

-- | Specifies which serializer to use. You can choose either the ORC SerDe
-- or the Parquet SerDe. If both are non-null, the server rejects the
-- request.
outputFormatConfiguration_serializer :: Lens.Lens' OutputFormatConfiguration (Core.Maybe Serializer)
outputFormatConfiguration_serializer = Lens.lens (\OutputFormatConfiguration' {serializer} -> serializer) (\s@OutputFormatConfiguration' {} a -> s {serializer = a} :: OutputFormatConfiguration)

instance Core.FromJSON OutputFormatConfiguration where
  parseJSON =
    Core.withObject
      "OutputFormatConfiguration"
      ( \x ->
          OutputFormatConfiguration'
            Core.<$> (x Core..:? "Serializer")
      )

instance Core.Hashable OutputFormatConfiguration

instance Core.NFData OutputFormatConfiguration

instance Core.ToJSON OutputFormatConfiguration where
  toJSON OutputFormatConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [("Serializer" Core..=) Core.<$> serializer]
      )
