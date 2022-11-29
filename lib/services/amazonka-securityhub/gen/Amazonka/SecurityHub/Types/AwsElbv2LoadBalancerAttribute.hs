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
-- Module      : Amazonka.SecurityHub.Types.AwsElbv2LoadBalancerAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbv2LoadBalancerAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A load balancer attribute.
--
-- /See:/ 'newAwsElbv2LoadBalancerAttribute' smart constructor.
data AwsElbv2LoadBalancerAttribute = AwsElbv2LoadBalancerAttribute'
  { -- | The name of the load balancer attribute.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value of the load balancer attribute.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElbv2LoadBalancerAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'awsElbv2LoadBalancerAttribute_key' - The name of the load balancer attribute.
--
-- 'value', 'awsElbv2LoadBalancerAttribute_value' - The value of the load balancer attribute.
newAwsElbv2LoadBalancerAttribute ::
  AwsElbv2LoadBalancerAttribute
newAwsElbv2LoadBalancerAttribute =
  AwsElbv2LoadBalancerAttribute'
    { key =
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the load balancer attribute.
awsElbv2LoadBalancerAttribute_key :: Lens.Lens' AwsElbv2LoadBalancerAttribute (Prelude.Maybe Prelude.Text)
awsElbv2LoadBalancerAttribute_key = Lens.lens (\AwsElbv2LoadBalancerAttribute' {key} -> key) (\s@AwsElbv2LoadBalancerAttribute' {} a -> s {key = a} :: AwsElbv2LoadBalancerAttribute)

-- | The value of the load balancer attribute.
awsElbv2LoadBalancerAttribute_value :: Lens.Lens' AwsElbv2LoadBalancerAttribute (Prelude.Maybe Prelude.Text)
awsElbv2LoadBalancerAttribute_value = Lens.lens (\AwsElbv2LoadBalancerAttribute' {value} -> value) (\s@AwsElbv2LoadBalancerAttribute' {} a -> s {value = a} :: AwsElbv2LoadBalancerAttribute)

instance Core.FromJSON AwsElbv2LoadBalancerAttribute where
  parseJSON =
    Core.withObject
      "AwsElbv2LoadBalancerAttribute"
      ( \x ->
          AwsElbv2LoadBalancerAttribute'
            Prelude.<$> (x Core..:? "Key") Prelude.<*> (x Core..:? "Value")
      )

instance
  Prelude.Hashable
    AwsElbv2LoadBalancerAttribute
  where
  hashWithSalt _salt AwsElbv2LoadBalancerAttribute' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData AwsElbv2LoadBalancerAttribute where
  rnf AwsElbv2LoadBalancerAttribute' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Core.ToJSON AwsElbv2LoadBalancerAttribute where
  toJSON AwsElbv2LoadBalancerAttribute' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Key" Core..=) Prelude.<$> key,
            ("Value" Core..=) Prelude.<$> value
          ]
      )
