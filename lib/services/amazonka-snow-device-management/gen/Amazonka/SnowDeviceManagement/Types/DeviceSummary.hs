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
-- Module      : Amazonka.SnowDeviceManagement.Types.DeviceSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Types.DeviceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifying information about the device.
--
-- /See:/ 'newDeviceSummary' smart constructor.
data DeviceSummary = DeviceSummary'
  { -- | The ID of the job used to order the device.
    associatedWithJob :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the device.
    managedDeviceArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the device.
    managedDeviceId :: Prelude.Maybe Prelude.Text,
    -- | Optional metadata that you assign to a resource. You can use tags to
    -- categorize a resource in different ways, such as by purpose, owner, or
    -- environment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedWithJob', 'deviceSummary_associatedWithJob' - The ID of the job used to order the device.
--
-- 'managedDeviceArn', 'deviceSummary_managedDeviceArn' - The Amazon Resource Name (ARN) of the device.
--
-- 'managedDeviceId', 'deviceSummary_managedDeviceId' - The ID of the device.
--
-- 'tags', 'deviceSummary_tags' - Optional metadata that you assign to a resource. You can use tags to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment.
newDeviceSummary ::
  DeviceSummary
newDeviceSummary =
  DeviceSummary'
    { associatedWithJob = Prelude.Nothing,
      managedDeviceArn = Prelude.Nothing,
      managedDeviceId = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ID of the job used to order the device.
deviceSummary_associatedWithJob :: Lens.Lens' DeviceSummary (Prelude.Maybe Prelude.Text)
deviceSummary_associatedWithJob = Lens.lens (\DeviceSummary' {associatedWithJob} -> associatedWithJob) (\s@DeviceSummary' {} a -> s {associatedWithJob = a} :: DeviceSummary)

-- | The Amazon Resource Name (ARN) of the device.
deviceSummary_managedDeviceArn :: Lens.Lens' DeviceSummary (Prelude.Maybe Prelude.Text)
deviceSummary_managedDeviceArn = Lens.lens (\DeviceSummary' {managedDeviceArn} -> managedDeviceArn) (\s@DeviceSummary' {} a -> s {managedDeviceArn = a} :: DeviceSummary)

-- | The ID of the device.
deviceSummary_managedDeviceId :: Lens.Lens' DeviceSummary (Prelude.Maybe Prelude.Text)
deviceSummary_managedDeviceId = Lens.lens (\DeviceSummary' {managedDeviceId} -> managedDeviceId) (\s@DeviceSummary' {} a -> s {managedDeviceId = a} :: DeviceSummary)

-- | Optional metadata that you assign to a resource. You can use tags to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment.
deviceSummary_tags :: Lens.Lens' DeviceSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
deviceSummary_tags = Lens.lens (\DeviceSummary' {tags} -> tags) (\s@DeviceSummary' {} a -> s {tags = a} :: DeviceSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DeviceSummary where
  parseJSON =
    Data.withObject
      "DeviceSummary"
      ( \x ->
          DeviceSummary'
            Prelude.<$> (x Data..:? "associatedWithJob")
            Prelude.<*> (x Data..:? "managedDeviceArn")
            Prelude.<*> (x Data..:? "managedDeviceId")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DeviceSummary where
  hashWithSalt _salt DeviceSummary' {..} =
    _salt `Prelude.hashWithSalt` associatedWithJob
      `Prelude.hashWithSalt` managedDeviceArn
      `Prelude.hashWithSalt` managedDeviceId
      `Prelude.hashWithSalt` tags

instance Prelude.NFData DeviceSummary where
  rnf DeviceSummary' {..} =
    Prelude.rnf associatedWithJob
      `Prelude.seq` Prelude.rnf managedDeviceArn
      `Prelude.seq` Prelude.rnf managedDeviceId
      `Prelude.seq` Prelude.rnf tags
