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
-- Module      : Amazonka.IoTRoboRunner.Types.VendorProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTRoboRunner.Types.VendorProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Properties of the worker that are provided by the vendor FMS.
--
-- /See:/ 'newVendorProperties' smart constructor.
data VendorProperties = VendorProperties'
  { vendorAdditionalFixedProperties :: Prelude.Maybe Prelude.Text,
    vendorAdditionalTransientProperties :: Prelude.Maybe Prelude.Text,
    vendorWorkerIpAddress :: Prelude.Maybe Prelude.Text,
    vendorWorkerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VendorProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vendorAdditionalFixedProperties', 'vendorProperties_vendorAdditionalFixedProperties' - Undocumented member.
--
-- 'vendorAdditionalTransientProperties', 'vendorProperties_vendorAdditionalTransientProperties' - Undocumented member.
--
-- 'vendorWorkerIpAddress', 'vendorProperties_vendorWorkerIpAddress' - Undocumented member.
--
-- 'vendorWorkerId', 'vendorProperties_vendorWorkerId' - Undocumented member.
newVendorProperties ::
  -- | 'vendorWorkerId'
  Prelude.Text ->
  VendorProperties
newVendorProperties pVendorWorkerId_ =
  VendorProperties'
    { vendorAdditionalFixedProperties =
        Prelude.Nothing,
      vendorAdditionalTransientProperties =
        Prelude.Nothing,
      vendorWorkerIpAddress = Prelude.Nothing,
      vendorWorkerId = pVendorWorkerId_
    }

-- | Undocumented member.
vendorProperties_vendorAdditionalFixedProperties :: Lens.Lens' VendorProperties (Prelude.Maybe Prelude.Text)
vendorProperties_vendorAdditionalFixedProperties = Lens.lens (\VendorProperties' {vendorAdditionalFixedProperties} -> vendorAdditionalFixedProperties) (\s@VendorProperties' {} a -> s {vendorAdditionalFixedProperties = a} :: VendorProperties)

-- | Undocumented member.
vendorProperties_vendorAdditionalTransientProperties :: Lens.Lens' VendorProperties (Prelude.Maybe Prelude.Text)
vendorProperties_vendorAdditionalTransientProperties = Lens.lens (\VendorProperties' {vendorAdditionalTransientProperties} -> vendorAdditionalTransientProperties) (\s@VendorProperties' {} a -> s {vendorAdditionalTransientProperties = a} :: VendorProperties)

-- | Undocumented member.
vendorProperties_vendorWorkerIpAddress :: Lens.Lens' VendorProperties (Prelude.Maybe Prelude.Text)
vendorProperties_vendorWorkerIpAddress = Lens.lens (\VendorProperties' {vendorWorkerIpAddress} -> vendorWorkerIpAddress) (\s@VendorProperties' {} a -> s {vendorWorkerIpAddress = a} :: VendorProperties)

-- | Undocumented member.
vendorProperties_vendorWorkerId :: Lens.Lens' VendorProperties Prelude.Text
vendorProperties_vendorWorkerId = Lens.lens (\VendorProperties' {vendorWorkerId} -> vendorWorkerId) (\s@VendorProperties' {} a -> s {vendorWorkerId = a} :: VendorProperties)

instance Data.FromJSON VendorProperties where
  parseJSON =
    Data.withObject
      "VendorProperties"
      ( \x ->
          VendorProperties'
            Prelude.<$> (x Data..:? "vendorAdditionalFixedProperties")
            Prelude.<*> (x Data..:? "vendorAdditionalTransientProperties")
            Prelude.<*> (x Data..:? "vendorWorkerIpAddress")
            Prelude.<*> (x Data..: "vendorWorkerId")
      )

instance Prelude.Hashable VendorProperties where
  hashWithSalt _salt VendorProperties' {..} =
    _salt
      `Prelude.hashWithSalt` vendorAdditionalFixedProperties
      `Prelude.hashWithSalt` vendorAdditionalTransientProperties
      `Prelude.hashWithSalt` vendorWorkerIpAddress
      `Prelude.hashWithSalt` vendorWorkerId

instance Prelude.NFData VendorProperties where
  rnf VendorProperties' {..} =
    Prelude.rnf vendorAdditionalFixedProperties
      `Prelude.seq` Prelude.rnf vendorAdditionalTransientProperties
      `Prelude.seq` Prelude.rnf vendorWorkerIpAddress
      `Prelude.seq` Prelude.rnf vendorWorkerId

instance Data.ToJSON VendorProperties where
  toJSON VendorProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("vendorAdditionalFixedProperties" Data..=)
              Prelude.<$> vendorAdditionalFixedProperties,
            ("vendorAdditionalTransientProperties" Data..=)
              Prelude.<$> vendorAdditionalTransientProperties,
            ("vendorWorkerIpAddress" Data..=)
              Prelude.<$> vendorWorkerIpAddress,
            Prelude.Just
              ("vendorWorkerId" Data..= vendorWorkerId)
          ]
      )
