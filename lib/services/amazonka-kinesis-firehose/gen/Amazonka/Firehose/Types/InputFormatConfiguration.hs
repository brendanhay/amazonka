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
-- Module      : Amazonka.Firehose.Types.InputFormatConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.InputFormatConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.Deserializer
import qualified Amazonka.Prelude as Prelude

-- | Specifies the deserializer you want to use to convert the format of the
-- input data. This parameter is required if @Enabled@ is set to true.
--
-- /See:/ 'newInputFormatConfiguration' smart constructor.
data InputFormatConfiguration = InputFormatConfiguration'
  { -- | Specifies which deserializer to use. You can choose either the Apache
    -- Hive JSON SerDe or the OpenX JSON SerDe. If both are non-null, the
    -- server rejects the request.
    deserializer :: Prelude.Maybe Deserializer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputFormatConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deserializer', 'inputFormatConfiguration_deserializer' - Specifies which deserializer to use. You can choose either the Apache
-- Hive JSON SerDe or the OpenX JSON SerDe. If both are non-null, the
-- server rejects the request.
newInputFormatConfiguration ::
  InputFormatConfiguration
newInputFormatConfiguration =
  InputFormatConfiguration'
    { deserializer =
        Prelude.Nothing
    }

-- | Specifies which deserializer to use. You can choose either the Apache
-- Hive JSON SerDe or the OpenX JSON SerDe. If both are non-null, the
-- server rejects the request.
inputFormatConfiguration_deserializer :: Lens.Lens' InputFormatConfiguration (Prelude.Maybe Deserializer)
inputFormatConfiguration_deserializer = Lens.lens (\InputFormatConfiguration' {deserializer} -> deserializer) (\s@InputFormatConfiguration' {} a -> s {deserializer = a} :: InputFormatConfiguration)

instance Data.FromJSON InputFormatConfiguration where
  parseJSON =
    Data.withObject
      "InputFormatConfiguration"
      ( \x ->
          InputFormatConfiguration'
            Prelude.<$> (x Data..:? "Deserializer")
      )

instance Prelude.Hashable InputFormatConfiguration where
  hashWithSalt _salt InputFormatConfiguration' {..} =
    _salt `Prelude.hashWithSalt` deserializer

instance Prelude.NFData InputFormatConfiguration where
  rnf InputFormatConfiguration' {..} =
    Prelude.rnf deserializer

instance Data.ToJSON InputFormatConfiguration where
  toJSON InputFormatConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Deserializer" Data..=) Prelude.<$> deserializer]
      )
