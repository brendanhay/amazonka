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
-- Module      : Amazonka.Nimble.Types.Eula
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.Eula where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a EULA resource.
--
-- /See:/ 'newEula' smart constructor.
data Eula = Eula'
  { -- | The EULA content.
    content :: Prelude.Maybe Prelude.Text,
    -- | The ISO timestamp in seconds for when the resource was created.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The EULA ID.
    eulaId :: Prelude.Maybe Prelude.Text,
    -- | The name for the EULA.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ISO timestamp in seconds for when the resource was updated.
    updatedAt :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Eula' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'eula_content' - The EULA content.
--
-- 'createdAt', 'eula_createdAt' - The ISO timestamp in seconds for when the resource was created.
--
-- 'eulaId', 'eula_eulaId' - The EULA ID.
--
-- 'name', 'eula_name' - The name for the EULA.
--
-- 'updatedAt', 'eula_updatedAt' - The ISO timestamp in seconds for when the resource was updated.
newEula ::
  Eula
newEula =
  Eula'
    { content = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      eulaId = Prelude.Nothing,
      name = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The EULA content.
eula_content :: Lens.Lens' Eula (Prelude.Maybe Prelude.Text)
eula_content = Lens.lens (\Eula' {content} -> content) (\s@Eula' {} a -> s {content = a} :: Eula)

-- | The ISO timestamp in seconds for when the resource was created.
eula_createdAt :: Lens.Lens' Eula (Prelude.Maybe Prelude.UTCTime)
eula_createdAt = Lens.lens (\Eula' {createdAt} -> createdAt) (\s@Eula' {} a -> s {createdAt = a} :: Eula) Prelude.. Lens.mapping Data._Time

-- | The EULA ID.
eula_eulaId :: Lens.Lens' Eula (Prelude.Maybe Prelude.Text)
eula_eulaId = Lens.lens (\Eula' {eulaId} -> eulaId) (\s@Eula' {} a -> s {eulaId = a} :: Eula)

-- | The name for the EULA.
eula_name :: Lens.Lens' Eula (Prelude.Maybe Prelude.Text)
eula_name = Lens.lens (\Eula' {name} -> name) (\s@Eula' {} a -> s {name = a} :: Eula)

-- | The ISO timestamp in seconds for when the resource was updated.
eula_updatedAt :: Lens.Lens' Eula (Prelude.Maybe Prelude.UTCTime)
eula_updatedAt = Lens.lens (\Eula' {updatedAt} -> updatedAt) (\s@Eula' {} a -> s {updatedAt = a} :: Eula) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Eula where
  parseJSON =
    Data.withObject
      "Eula"
      ( \x ->
          Eula'
            Prelude.<$> (x Data..:? "content")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "eulaId")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance Prelude.Hashable Eula where
  hashWithSalt _salt Eula' {..} =
    _salt
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` eulaId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData Eula where
  rnf Eula' {..} =
    Prelude.rnf content
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf eulaId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf updatedAt
