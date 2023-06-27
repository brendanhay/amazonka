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
-- Module      : Amazonka.EC2.Types.InstanceExportDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceExportDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ExportEnvironment
import qualified Amazonka.Prelude as Prelude

-- | Describes an instance to export.
--
-- /See:/ 'newInstanceExportDetails' smart constructor.
data InstanceExportDetails = InstanceExportDetails'
  { -- | The ID of the resource being exported.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The target virtualization environment.
    targetEnvironment :: Prelude.Maybe ExportEnvironment
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceExportDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'instanceExportDetails_instanceId' - The ID of the resource being exported.
--
-- 'targetEnvironment', 'instanceExportDetails_targetEnvironment' - The target virtualization environment.
newInstanceExportDetails ::
  InstanceExportDetails
newInstanceExportDetails =
  InstanceExportDetails'
    { instanceId =
        Prelude.Nothing,
      targetEnvironment = Prelude.Nothing
    }

-- | The ID of the resource being exported.
instanceExportDetails_instanceId :: Lens.Lens' InstanceExportDetails (Prelude.Maybe Prelude.Text)
instanceExportDetails_instanceId = Lens.lens (\InstanceExportDetails' {instanceId} -> instanceId) (\s@InstanceExportDetails' {} a -> s {instanceId = a} :: InstanceExportDetails)

-- | The target virtualization environment.
instanceExportDetails_targetEnvironment :: Lens.Lens' InstanceExportDetails (Prelude.Maybe ExportEnvironment)
instanceExportDetails_targetEnvironment = Lens.lens (\InstanceExportDetails' {targetEnvironment} -> targetEnvironment) (\s@InstanceExportDetails' {} a -> s {targetEnvironment = a} :: InstanceExportDetails)

instance Data.FromXML InstanceExportDetails where
  parseXML x =
    InstanceExportDetails'
      Prelude.<$> (x Data..@? "instanceId")
      Prelude.<*> (x Data..@? "targetEnvironment")

instance Prelude.Hashable InstanceExportDetails where
  hashWithSalt _salt InstanceExportDetails' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` targetEnvironment

instance Prelude.NFData InstanceExportDetails where
  rnf InstanceExportDetails' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf targetEnvironment
