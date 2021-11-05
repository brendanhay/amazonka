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
-- Module      : Network.AWS.Nimble.Types.Eula
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Nimble.Types.Eula where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a EULA resource.
--
-- /See:/ 'newEula' smart constructor.
data Eula = Eula'
  { -- | The Unix epoch timestamp in seconds for when the resource was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The EULA ID.
    eulaId :: Prelude.Maybe Prelude.Text,
    -- | The EULA content.
    content :: Prelude.Maybe Prelude.Text,
    -- | The name for the EULA.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was updated.
    updatedAt :: Prelude.Maybe Core.POSIX
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
-- 'createdAt', 'eula_createdAt' - The Unix epoch timestamp in seconds for when the resource was created.
--
-- 'eulaId', 'eula_eulaId' - The EULA ID.
--
-- 'content', 'eula_content' - The EULA content.
--
-- 'name', 'eula_name' - The name for the EULA.
--
-- 'updatedAt', 'eula_updatedAt' - The Unix epoch timestamp in seconds for when the resource was updated.
newEula ::
  Eula
newEula =
  Eula'
    { createdAt = Prelude.Nothing,
      eulaId = Prelude.Nothing,
      content = Prelude.Nothing,
      name = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The Unix epoch timestamp in seconds for when the resource was created.
eula_createdAt :: Lens.Lens' Eula (Prelude.Maybe Prelude.UTCTime)
eula_createdAt = Lens.lens (\Eula' {createdAt} -> createdAt) (\s@Eula' {} a -> s {createdAt = a} :: Eula) Prelude.. Lens.mapping Core._Time

-- | The EULA ID.
eula_eulaId :: Lens.Lens' Eula (Prelude.Maybe Prelude.Text)
eula_eulaId = Lens.lens (\Eula' {eulaId} -> eulaId) (\s@Eula' {} a -> s {eulaId = a} :: Eula)

-- | The EULA content.
eula_content :: Lens.Lens' Eula (Prelude.Maybe Prelude.Text)
eula_content = Lens.lens (\Eula' {content} -> content) (\s@Eula' {} a -> s {content = a} :: Eula)

-- | The name for the EULA.
eula_name :: Lens.Lens' Eula (Prelude.Maybe Prelude.Text)
eula_name = Lens.lens (\Eula' {name} -> name) (\s@Eula' {} a -> s {name = a} :: Eula)

-- | The Unix epoch timestamp in seconds for when the resource was updated.
eula_updatedAt :: Lens.Lens' Eula (Prelude.Maybe Prelude.UTCTime)
eula_updatedAt = Lens.lens (\Eula' {updatedAt} -> updatedAt) (\s@Eula' {} a -> s {updatedAt = a} :: Eula) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Eula where
  parseJSON =
    Core.withObject
      "Eula"
      ( \x ->
          Eula'
            Prelude.<$> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "eulaId")
            Prelude.<*> (x Core..:? "content")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "updatedAt")
      )

instance Prelude.Hashable Eula

instance Prelude.NFData Eula
