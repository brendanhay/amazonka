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
-- Module      : Network.AWS.ElasticInference.Types.KeyValuePair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticInference.Types.KeyValuePair where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A throughput entry for an Elastic Inference Accelerator type.
--
-- /See:/ 'newKeyValuePair' smart constructor.
data KeyValuePair = KeyValuePair'
  { -- | The throughput value of the Elastic Inference Accelerator type.
    value :: Prelude.Maybe Prelude.Int,
    -- | The throughput value of the Elastic Inference Accelerator type. It can
    -- assume the following values: TFLOPS16bit: the throughput expressed in
    -- 16bit TeraFLOPS. TFLOPS32bit: the throughput expressed in 32bit
    -- TeraFLOPS.
    key :: Prelude.Maybe Prelude.Text
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
-- 'value', 'keyValuePair_value' - The throughput value of the Elastic Inference Accelerator type.
--
-- 'key', 'keyValuePair_key' - The throughput value of the Elastic Inference Accelerator type. It can
-- assume the following values: TFLOPS16bit: the throughput expressed in
-- 16bit TeraFLOPS. TFLOPS32bit: the throughput expressed in 32bit
-- TeraFLOPS.
newKeyValuePair ::
  KeyValuePair
newKeyValuePair =
  KeyValuePair'
    { value = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The throughput value of the Elastic Inference Accelerator type.
keyValuePair_value :: Lens.Lens' KeyValuePair (Prelude.Maybe Prelude.Int)
keyValuePair_value = Lens.lens (\KeyValuePair' {value} -> value) (\s@KeyValuePair' {} a -> s {value = a} :: KeyValuePair)

-- | The throughput value of the Elastic Inference Accelerator type. It can
-- assume the following values: TFLOPS16bit: the throughput expressed in
-- 16bit TeraFLOPS. TFLOPS32bit: the throughput expressed in 32bit
-- TeraFLOPS.
keyValuePair_key :: Lens.Lens' KeyValuePair (Prelude.Maybe Prelude.Text)
keyValuePair_key = Lens.lens (\KeyValuePair' {key} -> key) (\s@KeyValuePair' {} a -> s {key = a} :: KeyValuePair)

instance Core.FromJSON KeyValuePair where
  parseJSON =
    Core.withObject
      "KeyValuePair"
      ( \x ->
          KeyValuePair'
            Prelude.<$> (x Core..:? "value") Prelude.<*> (x Core..:? "key")
      )

instance Prelude.Hashable KeyValuePair

instance Prelude.NFData KeyValuePair
