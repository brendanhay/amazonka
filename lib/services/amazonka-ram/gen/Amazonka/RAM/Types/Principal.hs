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
-- Module      : Amazonka.RAM.Types.Principal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.Principal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a principal for use with Resource Access Manager.
--
-- /See:/ 'newPrincipal' smart constructor.
data Principal = Principal'
  { -- | The time when the principal was associated with the resource share.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the resource share.
    resourceShareArn :: Prelude.Maybe Prelude.Text,
    -- | The time when the association was last updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | Indicates whether the principal belongs to the same organization in
    -- Organizations as the Amazon Web Services account that owns the resource
    -- share.
    external :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the principal.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Principal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'principal_creationTime' - The time when the principal was associated with the resource share.
--
-- 'resourceShareArn', 'principal_resourceShareArn' - The Amazon Resource Name (ARN) of the resource share.
--
-- 'lastUpdatedTime', 'principal_lastUpdatedTime' - The time when the association was last updated.
--
-- 'external', 'principal_external' - Indicates whether the principal belongs to the same organization in
-- Organizations as the Amazon Web Services account that owns the resource
-- share.
--
-- 'id', 'principal_id' - The ID of the principal.
newPrincipal ::
  Principal
newPrincipal =
  Principal'
    { creationTime = Prelude.Nothing,
      resourceShareArn = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      external = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The time when the principal was associated with the resource share.
principal_creationTime :: Lens.Lens' Principal (Prelude.Maybe Prelude.UTCTime)
principal_creationTime = Lens.lens (\Principal' {creationTime} -> creationTime) (\s@Principal' {} a -> s {creationTime = a} :: Principal) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the resource share.
principal_resourceShareArn :: Lens.Lens' Principal (Prelude.Maybe Prelude.Text)
principal_resourceShareArn = Lens.lens (\Principal' {resourceShareArn} -> resourceShareArn) (\s@Principal' {} a -> s {resourceShareArn = a} :: Principal)

-- | The time when the association was last updated.
principal_lastUpdatedTime :: Lens.Lens' Principal (Prelude.Maybe Prelude.UTCTime)
principal_lastUpdatedTime = Lens.lens (\Principal' {lastUpdatedTime} -> lastUpdatedTime) (\s@Principal' {} a -> s {lastUpdatedTime = a} :: Principal) Prelude.. Lens.mapping Core._Time

-- | Indicates whether the principal belongs to the same organization in
-- Organizations as the Amazon Web Services account that owns the resource
-- share.
principal_external :: Lens.Lens' Principal (Prelude.Maybe Prelude.Bool)
principal_external = Lens.lens (\Principal' {external} -> external) (\s@Principal' {} a -> s {external = a} :: Principal)

-- | The ID of the principal.
principal_id :: Lens.Lens' Principal (Prelude.Maybe Prelude.Text)
principal_id = Lens.lens (\Principal' {id} -> id) (\s@Principal' {} a -> s {id = a} :: Principal)

instance Core.FromJSON Principal where
  parseJSON =
    Core.withObject
      "Principal"
      ( \x ->
          Principal'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "resourceShareArn")
            Prelude.<*> (x Core..:? "lastUpdatedTime")
            Prelude.<*> (x Core..:? "external")
            Prelude.<*> (x Core..:? "id")
      )

instance Prelude.Hashable Principal where
  hashWithSalt _salt Principal' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` resourceShareArn
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` external
      `Prelude.hashWithSalt` id

instance Prelude.NFData Principal where
  rnf Principal' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf resourceShareArn
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf external
      `Prelude.seq` Prelude.rnf id
