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
-- Module      : Network.AWS.Config.Types.FieldInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.FieldInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details about the fields such as name of the field.
--
-- /See:/ 'newFieldInfo' smart constructor.
data FieldInfo = FieldInfo'
  { -- | Name of the field.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FieldInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'fieldInfo_name' - Name of the field.
newFieldInfo ::
  FieldInfo
newFieldInfo = FieldInfo' {name = Core.Nothing}

-- | Name of the field.
fieldInfo_name :: Lens.Lens' FieldInfo (Core.Maybe Core.Text)
fieldInfo_name = Lens.lens (\FieldInfo' {name} -> name) (\s@FieldInfo' {} a -> s {name = a} :: FieldInfo)

instance Core.FromJSON FieldInfo where
  parseJSON =
    Core.withObject
      "FieldInfo"
      (\x -> FieldInfo' Core.<$> (x Core..:? "Name"))

instance Core.Hashable FieldInfo

instance Core.NFData FieldInfo
