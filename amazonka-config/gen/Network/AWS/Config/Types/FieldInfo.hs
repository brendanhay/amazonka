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
-- Module      : Network.AWS.Config.Types.FieldInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.FieldInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about the fields such as name of the field.
--
-- /See:/ 'newFieldInfo' smart constructor.
data FieldInfo = FieldInfo'
  { -- | Name of the field.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
newFieldInfo = FieldInfo' {name = Prelude.Nothing}

-- | Name of the field.
fieldInfo_name :: Lens.Lens' FieldInfo (Prelude.Maybe Prelude.Text)
fieldInfo_name = Lens.lens (\FieldInfo' {name} -> name) (\s@FieldInfo' {} a -> s {name = a} :: FieldInfo)

instance Prelude.FromJSON FieldInfo where
  parseJSON =
    Prelude.withObject
      "FieldInfo"
      ( \x ->
          FieldInfo' Prelude.<$> (x Prelude..:? "Name")
      )

instance Prelude.Hashable FieldInfo

instance Prelude.NFData FieldInfo
