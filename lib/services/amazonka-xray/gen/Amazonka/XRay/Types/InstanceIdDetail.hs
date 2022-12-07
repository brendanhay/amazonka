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
-- Module      : Amazonka.XRay.Types.InstanceIdDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.InstanceIdDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of EC2 instance IDs corresponding to the segments in a trace.
--
-- /See:/ 'newInstanceIdDetail' smart constructor.
data InstanceIdDetail = InstanceIdDetail'
  { -- | The ID of a corresponding EC2 instance.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceIdDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'instanceIdDetail_id' - The ID of a corresponding EC2 instance.
newInstanceIdDetail ::
  InstanceIdDetail
newInstanceIdDetail =
  InstanceIdDetail' {id = Prelude.Nothing}

-- | The ID of a corresponding EC2 instance.
instanceIdDetail_id :: Lens.Lens' InstanceIdDetail (Prelude.Maybe Prelude.Text)
instanceIdDetail_id = Lens.lens (\InstanceIdDetail' {id} -> id) (\s@InstanceIdDetail' {} a -> s {id = a} :: InstanceIdDetail)

instance Data.FromJSON InstanceIdDetail where
  parseJSON =
    Data.withObject
      "InstanceIdDetail"
      ( \x ->
          InstanceIdDetail' Prelude.<$> (x Data..:? "Id")
      )

instance Prelude.Hashable InstanceIdDetail where
  hashWithSalt _salt InstanceIdDetail' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData InstanceIdDetail where
  rnf InstanceIdDetail' {..} = Prelude.rnf id
