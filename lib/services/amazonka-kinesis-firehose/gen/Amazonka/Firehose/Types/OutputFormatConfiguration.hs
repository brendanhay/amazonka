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
-- Module      : Amazonka.Firehose.Types.OutputFormatConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.OutputFormatConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.Serializer
import qualified Amazonka.Prelude as Prelude

-- | Specifies the serializer that you want Kinesis Data Firehose to use to
-- convert the format of your data before it writes it to Amazon S3. This
-- parameter is required if @Enabled@ is set to true.
--
-- /See:/ 'newOutputFormatConfiguration' smart constructor.
data OutputFormatConfiguration = OutputFormatConfiguration'
  { -- | Specifies which serializer to use. You can choose either the ORC SerDe
    -- or the Parquet SerDe. If both are non-null, the server rejects the
    -- request.
    serializer :: Prelude.Maybe Serializer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | Specifies which serializer to use. You can choose either the ORC SerDe
-- or the Parquet SerDe. If both are non-null, the server rejects the
-- request.
outputFormatConfiguration_serializer :: Lens.Lens' OutputFormatConfiguration (Prelude.Maybe Serializer)
outputFormatConfiguration_serializer = Lens.lens (\OutputFormatConfiguration' {serializer} -> serializer) (\s@OutputFormatConfiguration' {} a -> s {serializer = a} :: OutputFormatConfiguration)

instance Data.FromJSON OutputFormatConfiguration where
  parseJSON =
    Data.withObject
      "OutputFormatConfiguration"
      ( \x ->
          OutputFormatConfiguration'
            Prelude.<$> (x Data..:? "Serializer")
      )

instance Prelude.Hashable OutputFormatConfiguration where
  hashWithSalt _salt OutputFormatConfiguration' {..} =
    _salt `Prelude.hashWithSalt` serializer

instance Prelude.NFData OutputFormatConfiguration where
  rnf OutputFormatConfiguration' {..} =
    Prelude.rnf serializer

instance Data.ToJSON OutputFormatConfiguration where
  toJSON OutputFormatConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Serializer" Data..=) Prelude.<$> serializer]
      )
