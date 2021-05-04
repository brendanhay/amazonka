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
-- Module      : Network.AWS.Shield.Types.Limit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Limit where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies how many protections of a given type you can create.
--
-- /See:/ 'newLimit' smart constructor.
data Limit = Limit'
  { -- | The maximum number of protections that can be created for the specified
    -- @Type@.
    max :: Prelude.Maybe Prelude.Integer,
    -- | The type of protection.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Limit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'limit_max' - The maximum number of protections that can be created for the specified
-- @Type@.
--
-- 'type'', 'limit_type' - The type of protection.
newLimit ::
  Limit
newLimit =
  Limit'
    { max = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The maximum number of protections that can be created for the specified
-- @Type@.
limit_max :: Lens.Lens' Limit (Prelude.Maybe Prelude.Integer)
limit_max = Lens.lens (\Limit' {max} -> max) (\s@Limit' {} a -> s {max = a} :: Limit)

-- | The type of protection.
limit_type :: Lens.Lens' Limit (Prelude.Maybe Prelude.Text)
limit_type = Lens.lens (\Limit' {type'} -> type') (\s@Limit' {} a -> s {type' = a} :: Limit)

instance Prelude.FromJSON Limit where
  parseJSON =
    Prelude.withObject
      "Limit"
      ( \x ->
          Limit'
            Prelude.<$> (x Prelude..:? "Max")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable Limit

instance Prelude.NFData Limit
