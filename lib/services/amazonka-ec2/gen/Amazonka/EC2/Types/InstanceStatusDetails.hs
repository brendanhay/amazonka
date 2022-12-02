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
-- Module      : Amazonka.EC2.Types.InstanceStatusDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceStatusDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.StatusName
import Amazonka.EC2.Types.StatusType
import qualified Amazonka.Prelude as Prelude

-- | Describes the instance status.
--
-- /See:/ 'newInstanceStatusDetails' smart constructor.
data InstanceStatusDetails = InstanceStatusDetails'
  { -- | The time when a status check failed. For an instance that was launched
    -- and impaired, this is the time when the instance was launched.
    impairedSince :: Prelude.Maybe Data.ISO8601,
    -- | The type of instance status.
    name :: Prelude.Maybe StatusName,
    -- | The status.
    status :: Prelude.Maybe StatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceStatusDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'impairedSince', 'instanceStatusDetails_impairedSince' - The time when a status check failed. For an instance that was launched
-- and impaired, this is the time when the instance was launched.
--
-- 'name', 'instanceStatusDetails_name' - The type of instance status.
--
-- 'status', 'instanceStatusDetails_status' - The status.
newInstanceStatusDetails ::
  InstanceStatusDetails
newInstanceStatusDetails =
  InstanceStatusDetails'
    { impairedSince =
        Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The time when a status check failed. For an instance that was launched
-- and impaired, this is the time when the instance was launched.
instanceStatusDetails_impairedSince :: Lens.Lens' InstanceStatusDetails (Prelude.Maybe Prelude.UTCTime)
instanceStatusDetails_impairedSince = Lens.lens (\InstanceStatusDetails' {impairedSince} -> impairedSince) (\s@InstanceStatusDetails' {} a -> s {impairedSince = a} :: InstanceStatusDetails) Prelude.. Lens.mapping Data._Time

-- | The type of instance status.
instanceStatusDetails_name :: Lens.Lens' InstanceStatusDetails (Prelude.Maybe StatusName)
instanceStatusDetails_name = Lens.lens (\InstanceStatusDetails' {name} -> name) (\s@InstanceStatusDetails' {} a -> s {name = a} :: InstanceStatusDetails)

-- | The status.
instanceStatusDetails_status :: Lens.Lens' InstanceStatusDetails (Prelude.Maybe StatusType)
instanceStatusDetails_status = Lens.lens (\InstanceStatusDetails' {status} -> status) (\s@InstanceStatusDetails' {} a -> s {status = a} :: InstanceStatusDetails)

instance Data.FromXML InstanceStatusDetails where
  parseXML x =
    InstanceStatusDetails'
      Prelude.<$> (x Data..@? "impairedSince")
      Prelude.<*> (x Data..@? "name")
      Prelude.<*> (x Data..@? "status")

instance Prelude.Hashable InstanceStatusDetails where
  hashWithSalt _salt InstanceStatusDetails' {..} =
    _salt `Prelude.hashWithSalt` impairedSince
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData InstanceStatusDetails where
  rnf InstanceStatusDetails' {..} =
    Prelude.rnf impairedSince
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
