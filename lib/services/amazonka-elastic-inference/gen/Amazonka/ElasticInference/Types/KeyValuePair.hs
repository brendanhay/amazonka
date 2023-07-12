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
-- Module      : Amazonka.ElasticInference.Types.KeyValuePair
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticInference.Types.KeyValuePair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A throughput entry for an Elastic Inference Accelerator type.
--
-- /See:/ 'newKeyValuePair' smart constructor.
data KeyValuePair = KeyValuePair'
  { -- | The throughput value of the Elastic Inference Accelerator type. It can
    -- assume the following values: TFLOPS16bit: the throughput expressed in
    -- 16bit TeraFLOPS. TFLOPS32bit: the throughput expressed in 32bit
    -- TeraFLOPS.
    key :: Prelude.Maybe Prelude.Text,
    -- | The throughput value of the Elastic Inference Accelerator type.
    value :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeyValuePair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'keyValuePair_key' - The throughput value of the Elastic Inference Accelerator type. It can
-- assume the following values: TFLOPS16bit: the throughput expressed in
-- 16bit TeraFLOPS. TFLOPS32bit: the throughput expressed in 32bit
-- TeraFLOPS.
--
-- 'value', 'keyValuePair_value' - The throughput value of the Elastic Inference Accelerator type.
newKeyValuePair ::
  KeyValuePair
newKeyValuePair =
  KeyValuePair'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The throughput value of the Elastic Inference Accelerator type. It can
-- assume the following values: TFLOPS16bit: the throughput expressed in
-- 16bit TeraFLOPS. TFLOPS32bit: the throughput expressed in 32bit
-- TeraFLOPS.
keyValuePair_key :: Lens.Lens' KeyValuePair (Prelude.Maybe Prelude.Text)
keyValuePair_key = Lens.lens (\KeyValuePair' {key} -> key) (\s@KeyValuePair' {} a -> s {key = a} :: KeyValuePair)

-- | The throughput value of the Elastic Inference Accelerator type.
keyValuePair_value :: Lens.Lens' KeyValuePair (Prelude.Maybe Prelude.Int)
keyValuePair_value = Lens.lens (\KeyValuePair' {value} -> value) (\s@KeyValuePair' {} a -> s {value = a} :: KeyValuePair)

instance Data.FromJSON KeyValuePair where
  parseJSON =
    Data.withObject
      "KeyValuePair"
      ( \x ->
          KeyValuePair'
            Prelude.<$> (x Data..:? "key")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable KeyValuePair where
  hashWithSalt _salt KeyValuePair' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData KeyValuePair where
  rnf KeyValuePair' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value
