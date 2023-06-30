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
-- Module      : Amazonka.SecurityHub.Types.AwsElbLoadBalancerAdditionalAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbLoadBalancerAdditionalAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about additional attributes for the load balancer.
--
-- /See:/ 'newAwsElbLoadBalancerAdditionalAttribute' smart constructor.
data AwsElbLoadBalancerAdditionalAttribute = AwsElbLoadBalancerAdditionalAttribute'
  { -- | The name of the attribute.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value of the attribute.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElbLoadBalancerAdditionalAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'awsElbLoadBalancerAdditionalAttribute_key' - The name of the attribute.
--
-- 'value', 'awsElbLoadBalancerAdditionalAttribute_value' - The value of the attribute.
newAwsElbLoadBalancerAdditionalAttribute ::
  AwsElbLoadBalancerAdditionalAttribute
newAwsElbLoadBalancerAdditionalAttribute =
  AwsElbLoadBalancerAdditionalAttribute'
    { key =
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the attribute.
awsElbLoadBalancerAdditionalAttribute_key :: Lens.Lens' AwsElbLoadBalancerAdditionalAttribute (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerAdditionalAttribute_key = Lens.lens (\AwsElbLoadBalancerAdditionalAttribute' {key} -> key) (\s@AwsElbLoadBalancerAdditionalAttribute' {} a -> s {key = a} :: AwsElbLoadBalancerAdditionalAttribute)

-- | The value of the attribute.
awsElbLoadBalancerAdditionalAttribute_value :: Lens.Lens' AwsElbLoadBalancerAdditionalAttribute (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerAdditionalAttribute_value = Lens.lens (\AwsElbLoadBalancerAdditionalAttribute' {value} -> value) (\s@AwsElbLoadBalancerAdditionalAttribute' {} a -> s {value = a} :: AwsElbLoadBalancerAdditionalAttribute)

instance
  Data.FromJSON
    AwsElbLoadBalancerAdditionalAttribute
  where
  parseJSON =
    Data.withObject
      "AwsElbLoadBalancerAdditionalAttribute"
      ( \x ->
          AwsElbLoadBalancerAdditionalAttribute'
            Prelude.<$> (x Data..:? "Key")
            Prelude.<*> (x Data..:? "Value")
      )

instance
  Prelude.Hashable
    AwsElbLoadBalancerAdditionalAttribute
  where
  hashWithSalt
    _salt
    AwsElbLoadBalancerAdditionalAttribute' {..} =
      _salt
        `Prelude.hashWithSalt` key
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    AwsElbLoadBalancerAdditionalAttribute
  where
  rnf AwsElbLoadBalancerAdditionalAttribute' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance
  Data.ToJSON
    AwsElbLoadBalancerAdditionalAttribute
  where
  toJSON AwsElbLoadBalancerAdditionalAttribute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Key" Data..=) Prelude.<$> key,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
