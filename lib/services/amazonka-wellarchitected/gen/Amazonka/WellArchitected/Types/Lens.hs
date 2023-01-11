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
-- Module      : Amazonka.WellArchitected.Types.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.Lens where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A lens return object.
--
-- /See:/ 'newLens' smart constructor.
data Lens = Lens'
  { description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a lens.
    lensArn :: Prelude.Maybe Prelude.Text,
    -- | The version of a lens.
    lensVersion :: Prelude.Maybe Prelude.Text,
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID that owns the lens.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The ID assigned to the share invitation.
    shareInvitationId :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the lens.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Lens' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'lens_description' - Undocumented member.
--
-- 'lensArn', 'lens_lensArn' - The ARN of a lens.
--
-- 'lensVersion', 'lens_lensVersion' - The version of a lens.
--
-- 'name', 'lens_name' - Undocumented member.
--
-- 'owner', 'lens_owner' - The Amazon Web Services account ID that owns the lens.
--
-- 'shareInvitationId', 'lens_shareInvitationId' - The ID assigned to the share invitation.
--
-- 'tags', 'lens_tags' - The tags assigned to the lens.
newLens ::
  Lens
newLens =
  Lens'
    { description = Prelude.Nothing,
      lensArn = Prelude.Nothing,
      lensVersion = Prelude.Nothing,
      name = Prelude.Nothing,
      owner = Prelude.Nothing,
      shareInvitationId = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Undocumented member.
lens_description :: Lens.Lens' Lens (Prelude.Maybe Prelude.Text)
lens_description = Lens.lens (\Lens' {description} -> description) (\s@Lens' {} a -> s {description = a} :: Lens)

-- | The ARN of a lens.
lens_lensArn :: Lens.Lens' Lens (Prelude.Maybe Prelude.Text)
lens_lensArn = Lens.lens (\Lens' {lensArn} -> lensArn) (\s@Lens' {} a -> s {lensArn = a} :: Lens)

-- | The version of a lens.
lens_lensVersion :: Lens.Lens' Lens (Prelude.Maybe Prelude.Text)
lens_lensVersion = Lens.lens (\Lens' {lensVersion} -> lensVersion) (\s@Lens' {} a -> s {lensVersion = a} :: Lens)

-- | Undocumented member.
lens_name :: Lens.Lens' Lens (Prelude.Maybe Prelude.Text)
lens_name = Lens.lens (\Lens' {name} -> name) (\s@Lens' {} a -> s {name = a} :: Lens)

-- | The Amazon Web Services account ID that owns the lens.
lens_owner :: Lens.Lens' Lens (Prelude.Maybe Prelude.Text)
lens_owner = Lens.lens (\Lens' {owner} -> owner) (\s@Lens' {} a -> s {owner = a} :: Lens)

-- | The ID assigned to the share invitation.
lens_shareInvitationId :: Lens.Lens' Lens (Prelude.Maybe Prelude.Text)
lens_shareInvitationId = Lens.lens (\Lens' {shareInvitationId} -> shareInvitationId) (\s@Lens' {} a -> s {shareInvitationId = a} :: Lens)

-- | The tags assigned to the lens.
lens_tags :: Lens.Lens' Lens (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
lens_tags = Lens.lens (\Lens' {tags} -> tags) (\s@Lens' {} a -> s {tags = a} :: Lens) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Lens where
  parseJSON =
    Data.withObject
      "Lens"
      ( \x ->
          Lens'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LensArn")
            Prelude.<*> (x Data..:? "LensVersion")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Owner")
            Prelude.<*> (x Data..:? "ShareInvitationId")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Lens where
  hashWithSalt _salt Lens' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lensArn
      `Prelude.hashWithSalt` lensVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` shareInvitationId
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Lens where
  rnf Lens' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf lensArn
      `Prelude.seq` Prelude.rnf lensVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf shareInvitationId
      `Prelude.seq` Prelude.rnf tags
