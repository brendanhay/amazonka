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
-- Module      : Amazonka.SecurityHub.Types.AwsWafv2CustomHttpHeader
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafv2CustomHttpHeader where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A custom header for custom request and response handling.
--
-- /See:/ 'newAwsWafv2CustomHttpHeader' smart constructor.
data AwsWafv2CustomHttpHeader = AwsWafv2CustomHttpHeader'
  { -- | The name of the custom header.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value of the custom header.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafv2CustomHttpHeader' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'awsWafv2CustomHttpHeader_name' - The name of the custom header.
--
-- 'value', 'awsWafv2CustomHttpHeader_value' - The value of the custom header.
newAwsWafv2CustomHttpHeader ::
  AwsWafv2CustomHttpHeader
newAwsWafv2CustomHttpHeader =
  AwsWafv2CustomHttpHeader'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the custom header.
awsWafv2CustomHttpHeader_name :: Lens.Lens' AwsWafv2CustomHttpHeader (Prelude.Maybe Prelude.Text)
awsWafv2CustomHttpHeader_name = Lens.lens (\AwsWafv2CustomHttpHeader' {name} -> name) (\s@AwsWafv2CustomHttpHeader' {} a -> s {name = a} :: AwsWafv2CustomHttpHeader)

-- | The value of the custom header.
awsWafv2CustomHttpHeader_value :: Lens.Lens' AwsWafv2CustomHttpHeader (Prelude.Maybe Prelude.Text)
awsWafv2CustomHttpHeader_value = Lens.lens (\AwsWafv2CustomHttpHeader' {value} -> value) (\s@AwsWafv2CustomHttpHeader' {} a -> s {value = a} :: AwsWafv2CustomHttpHeader)

instance Data.FromJSON AwsWafv2CustomHttpHeader where
  parseJSON =
    Data.withObject
      "AwsWafv2CustomHttpHeader"
      ( \x ->
          AwsWafv2CustomHttpHeader'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable AwsWafv2CustomHttpHeader where
  hashWithSalt _salt AwsWafv2CustomHttpHeader' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData AwsWafv2CustomHttpHeader where
  rnf AwsWafv2CustomHttpHeader' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON AwsWafv2CustomHttpHeader where
  toJSON AwsWafv2CustomHttpHeader' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
