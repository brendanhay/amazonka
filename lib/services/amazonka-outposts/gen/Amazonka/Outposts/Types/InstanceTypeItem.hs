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
-- Module      : Amazonka.Outposts.Types.InstanceTypeItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.InstanceTypeItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an instance type.
--
-- /See:/ 'newInstanceTypeItem' smart constructor.
data InstanceTypeItem = InstanceTypeItem'
  { instanceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceTypeItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'instanceTypeItem_instanceType' - Undocumented member.
newInstanceTypeItem ::
  InstanceTypeItem
newInstanceTypeItem =
  InstanceTypeItem' {instanceType = Prelude.Nothing}

-- | Undocumented member.
instanceTypeItem_instanceType :: Lens.Lens' InstanceTypeItem (Prelude.Maybe Prelude.Text)
instanceTypeItem_instanceType = Lens.lens (\InstanceTypeItem' {instanceType} -> instanceType) (\s@InstanceTypeItem' {} a -> s {instanceType = a} :: InstanceTypeItem)

instance Core.FromJSON InstanceTypeItem where
  parseJSON =
    Core.withObject
      "InstanceTypeItem"
      ( \x ->
          InstanceTypeItem'
            Prelude.<$> (x Core..:? "InstanceType")
      )

instance Prelude.Hashable InstanceTypeItem where
  hashWithSalt _salt InstanceTypeItem' {..} =
    _salt `Prelude.hashWithSalt` instanceType

instance Prelude.NFData InstanceTypeItem where
  rnf InstanceTypeItem' {..} = Prelude.rnf instanceType
