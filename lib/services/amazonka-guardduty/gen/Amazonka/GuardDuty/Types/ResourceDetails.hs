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
-- Module      : Amazonka.GuardDuty.Types.ResourceDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ResourceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the resources that were scanned in the scan entry.
--
-- /See:/ 'newResourceDetails' smart constructor.
data ResourceDetails = ResourceDetails'
  { -- | InstanceArn that was scanned in the scan entry.
    instanceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceArn', 'resourceDetails_instanceArn' - InstanceArn that was scanned in the scan entry.
newResourceDetails ::
  ResourceDetails
newResourceDetails =
  ResourceDetails' {instanceArn = Prelude.Nothing}

-- | InstanceArn that was scanned in the scan entry.
resourceDetails_instanceArn :: Lens.Lens' ResourceDetails (Prelude.Maybe Prelude.Text)
resourceDetails_instanceArn = Lens.lens (\ResourceDetails' {instanceArn} -> instanceArn) (\s@ResourceDetails' {} a -> s {instanceArn = a} :: ResourceDetails)

instance Core.FromJSON ResourceDetails where
  parseJSON =
    Core.withObject
      "ResourceDetails"
      ( \x ->
          ResourceDetails'
            Prelude.<$> (x Core..:? "instanceArn")
      )

instance Prelude.Hashable ResourceDetails where
  hashWithSalt _salt ResourceDetails' {..} =
    _salt `Prelude.hashWithSalt` instanceArn

instance Prelude.NFData ResourceDetails where
  rnf ResourceDetails' {..} = Prelude.rnf instanceArn
