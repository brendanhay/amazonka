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
-- Module      : Network.AWS.SageMaker.Types.ProvisioningParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProvisioningParameter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A key value pair used when you provision a project as a service catalog
-- product. For information, see
-- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is AWS Service Catalog>.
--
-- /See:/ 'newProvisioningParameter' smart constructor.
data ProvisioningParameter = ProvisioningParameter'
  { -- | The key that identifies a provisioning parameter.
    key :: Core.Maybe Core.Text,
    -- | The value of the provisioning parameter.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProvisioningParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'provisioningParameter_key' - The key that identifies a provisioning parameter.
--
-- 'value', 'provisioningParameter_value' - The value of the provisioning parameter.
newProvisioningParameter ::
  ProvisioningParameter
newProvisioningParameter =
  ProvisioningParameter'
    { key = Core.Nothing,
      value = Core.Nothing
    }

-- | The key that identifies a provisioning parameter.
provisioningParameter_key :: Lens.Lens' ProvisioningParameter (Core.Maybe Core.Text)
provisioningParameter_key = Lens.lens (\ProvisioningParameter' {key} -> key) (\s@ProvisioningParameter' {} a -> s {key = a} :: ProvisioningParameter)

-- | The value of the provisioning parameter.
provisioningParameter_value :: Lens.Lens' ProvisioningParameter (Core.Maybe Core.Text)
provisioningParameter_value = Lens.lens (\ProvisioningParameter' {value} -> value) (\s@ProvisioningParameter' {} a -> s {value = a} :: ProvisioningParameter)

instance Core.FromJSON ProvisioningParameter where
  parseJSON =
    Core.withObject
      "ProvisioningParameter"
      ( \x ->
          ProvisioningParameter'
            Core.<$> (x Core..:? "Key") Core.<*> (x Core..:? "Value")
      )

instance Core.Hashable ProvisioningParameter

instance Core.NFData ProvisioningParameter

instance Core.ToJSON ProvisioningParameter where
  toJSON ProvisioningParameter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Value" Core..=) Core.<$> value
          ]
      )
