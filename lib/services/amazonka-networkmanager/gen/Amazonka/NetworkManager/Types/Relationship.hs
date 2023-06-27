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
-- Module      : Amazonka.NetworkManager.Types.Relationship
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.Relationship where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a resource relationship.
--
-- /See:/ 'newRelationship' smart constructor.
data Relationship = Relationship'
  { -- | The ARN of the resource.
    from :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the resource.
    to :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Relationship' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'from', 'relationship_from' - The ARN of the resource.
--
-- 'to', 'relationship_to' - The ARN of the resource.
newRelationship ::
  Relationship
newRelationship =
  Relationship'
    { from = Prelude.Nothing,
      to = Prelude.Nothing
    }

-- | The ARN of the resource.
relationship_from :: Lens.Lens' Relationship (Prelude.Maybe Prelude.Text)
relationship_from = Lens.lens (\Relationship' {from} -> from) (\s@Relationship' {} a -> s {from = a} :: Relationship)

-- | The ARN of the resource.
relationship_to :: Lens.Lens' Relationship (Prelude.Maybe Prelude.Text)
relationship_to = Lens.lens (\Relationship' {to} -> to) (\s@Relationship' {} a -> s {to = a} :: Relationship)

instance Data.FromJSON Relationship where
  parseJSON =
    Data.withObject
      "Relationship"
      ( \x ->
          Relationship'
            Prelude.<$> (x Data..:? "From")
            Prelude.<*> (x Data..:? "To")
      )

instance Prelude.Hashable Relationship where
  hashWithSalt _salt Relationship' {..} =
    _salt
      `Prelude.hashWithSalt` from
      `Prelude.hashWithSalt` to

instance Prelude.NFData Relationship where
  rnf Relationship' {..} =
    Prelude.rnf from `Prelude.seq` Prelude.rnf to
