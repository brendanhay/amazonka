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
-- Module      : Amazonka.GreengrassV2.Types.AssociatedClientDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.AssociatedClientDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a client device that is associated to a core
-- device for cloud discovery.
--
-- /See:/ 'newAssociatedClientDevice' smart constructor.
data AssociatedClientDevice = AssociatedClientDevice'
  { -- | The time that the client device was associated, expressed in ISO 8601
    -- format.
    associationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The name of the IoT thing that represents the associated client device.
    thingName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatedClientDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationTimestamp', 'associatedClientDevice_associationTimestamp' - The time that the client device was associated, expressed in ISO 8601
-- format.
--
-- 'thingName', 'associatedClientDevice_thingName' - The name of the IoT thing that represents the associated client device.
newAssociatedClientDevice ::
  AssociatedClientDevice
newAssociatedClientDevice =
  AssociatedClientDevice'
    { associationTimestamp =
        Prelude.Nothing,
      thingName = Prelude.Nothing
    }

-- | The time that the client device was associated, expressed in ISO 8601
-- format.
associatedClientDevice_associationTimestamp :: Lens.Lens' AssociatedClientDevice (Prelude.Maybe Prelude.UTCTime)
associatedClientDevice_associationTimestamp = Lens.lens (\AssociatedClientDevice' {associationTimestamp} -> associationTimestamp) (\s@AssociatedClientDevice' {} a -> s {associationTimestamp = a} :: AssociatedClientDevice) Prelude.. Lens.mapping Data._Time

-- | The name of the IoT thing that represents the associated client device.
associatedClientDevice_thingName :: Lens.Lens' AssociatedClientDevice (Prelude.Maybe Prelude.Text)
associatedClientDevice_thingName = Lens.lens (\AssociatedClientDevice' {thingName} -> thingName) (\s@AssociatedClientDevice' {} a -> s {thingName = a} :: AssociatedClientDevice)

instance Data.FromJSON AssociatedClientDevice where
  parseJSON =
    Data.withObject
      "AssociatedClientDevice"
      ( \x ->
          AssociatedClientDevice'
            Prelude.<$> (x Data..:? "associationTimestamp")
            Prelude.<*> (x Data..:? "thingName")
      )

instance Prelude.Hashable AssociatedClientDevice where
  hashWithSalt _salt AssociatedClientDevice' {..} =
    _salt `Prelude.hashWithSalt` associationTimestamp
      `Prelude.hashWithSalt` thingName

instance Prelude.NFData AssociatedClientDevice where
  rnf AssociatedClientDevice' {..} =
    Prelude.rnf associationTimestamp
      `Prelude.seq` Prelude.rnf thingName
