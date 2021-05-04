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
-- Module      : Network.AWS.Rekognition.Types.Parent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Parent where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A parent label for a label. A label can have 0, 1, or more parents.
--
-- /See:/ 'newParent' smart constructor.
data Parent = Parent'
  { -- | The name of the parent label.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Parent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'parent_name' - The name of the parent label.
newParent ::
  Parent
newParent = Parent' {name = Prelude.Nothing}

-- | The name of the parent label.
parent_name :: Lens.Lens' Parent (Prelude.Maybe Prelude.Text)
parent_name = Lens.lens (\Parent' {name} -> name) (\s@Parent' {} a -> s {name = a} :: Parent)

instance Prelude.FromJSON Parent where
  parseJSON =
    Prelude.withObject
      "Parent"
      (\x -> Parent' Prelude.<$> (x Prelude..:? "Name"))

instance Prelude.Hashable Parent

instance Prelude.NFData Parent
