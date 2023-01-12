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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbProcessorFeature
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbProcessorFeature where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A processor feature.
--
-- /See:/ 'newAwsRdsDbProcessorFeature' smart constructor.
data AwsRdsDbProcessorFeature = AwsRdsDbProcessorFeature'
  { -- | The name of the processor feature. Valid values are @coreCount@ or
    -- @threadsPerCore@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value of the processor feature.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbProcessorFeature' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'awsRdsDbProcessorFeature_name' - The name of the processor feature. Valid values are @coreCount@ or
-- @threadsPerCore@.
--
-- 'value', 'awsRdsDbProcessorFeature_value' - The value of the processor feature.
newAwsRdsDbProcessorFeature ::
  AwsRdsDbProcessorFeature
newAwsRdsDbProcessorFeature =
  AwsRdsDbProcessorFeature'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the processor feature. Valid values are @coreCount@ or
-- @threadsPerCore@.
awsRdsDbProcessorFeature_name :: Lens.Lens' AwsRdsDbProcessorFeature (Prelude.Maybe Prelude.Text)
awsRdsDbProcessorFeature_name = Lens.lens (\AwsRdsDbProcessorFeature' {name} -> name) (\s@AwsRdsDbProcessorFeature' {} a -> s {name = a} :: AwsRdsDbProcessorFeature)

-- | The value of the processor feature.
awsRdsDbProcessorFeature_value :: Lens.Lens' AwsRdsDbProcessorFeature (Prelude.Maybe Prelude.Text)
awsRdsDbProcessorFeature_value = Lens.lens (\AwsRdsDbProcessorFeature' {value} -> value) (\s@AwsRdsDbProcessorFeature' {} a -> s {value = a} :: AwsRdsDbProcessorFeature)

instance Data.FromJSON AwsRdsDbProcessorFeature where
  parseJSON =
    Data.withObject
      "AwsRdsDbProcessorFeature"
      ( \x ->
          AwsRdsDbProcessorFeature'
            Prelude.<$> (x Data..:? "Name") Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable AwsRdsDbProcessorFeature where
  hashWithSalt _salt AwsRdsDbProcessorFeature' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData AwsRdsDbProcessorFeature where
  rnf AwsRdsDbProcessorFeature' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON AwsRdsDbProcessorFeature where
  toJSON AwsRdsDbProcessorFeature' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
