{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A key value pair used when you provision a project as a service catalog
-- product. For information, see
-- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is AWS Service Catalog>.
--
-- /See:/ 'newProvisioningParameter' smart constructor.
data ProvisioningParameter = ProvisioningParameter'
  { -- | The key that identifies a provisioning parameter.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value of the provisioning parameter.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The key that identifies a provisioning parameter.
provisioningParameter_key :: Lens.Lens' ProvisioningParameter (Prelude.Maybe Prelude.Text)
provisioningParameter_key = Lens.lens (\ProvisioningParameter' {key} -> key) (\s@ProvisioningParameter' {} a -> s {key = a} :: ProvisioningParameter)

-- | The value of the provisioning parameter.
provisioningParameter_value :: Lens.Lens' ProvisioningParameter (Prelude.Maybe Prelude.Text)
provisioningParameter_value = Lens.lens (\ProvisioningParameter' {value} -> value) (\s@ProvisioningParameter' {} a -> s {value = a} :: ProvisioningParameter)

instance Prelude.FromJSON ProvisioningParameter where
  parseJSON =
    Prelude.withObject
      "ProvisioningParameter"
      ( \x ->
          ProvisioningParameter'
            Prelude.<$> (x Prelude..:? "Key")
            Prelude.<*> (x Prelude..:? "Value")
      )

instance Prelude.Hashable ProvisioningParameter

instance Prelude.NFData ProvisioningParameter

instance Prelude.ToJSON ProvisioningParameter where
  toJSON ProvisioningParameter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Key" Prelude..=) Prelude.<$> key,
            ("Value" Prelude..=) Prelude.<$> value
          ]
      )
