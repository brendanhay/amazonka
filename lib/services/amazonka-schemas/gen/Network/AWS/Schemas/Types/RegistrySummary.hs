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
-- Module      : Network.AWS.Schemas.Types.RegistrySummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Schemas.Types.RegistrySummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newRegistrySummary' smart constructor.
data RegistrySummary = RegistrySummary'
  { -- | The name of the registry.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the registry.
    registryArn :: Prelude.Maybe Prelude.Text,
    -- | Tags associated with the registry.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegistrySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryName', 'registrySummary_registryName' - The name of the registry.
--
-- 'registryArn', 'registrySummary_registryArn' - The ARN of the registry.
--
-- 'tags', 'registrySummary_tags' - Tags associated with the registry.
newRegistrySummary ::
  RegistrySummary
newRegistrySummary =
  RegistrySummary'
    { registryName = Prelude.Nothing,
      registryArn = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The name of the registry.
registrySummary_registryName :: Lens.Lens' RegistrySummary (Prelude.Maybe Prelude.Text)
registrySummary_registryName = Lens.lens (\RegistrySummary' {registryName} -> registryName) (\s@RegistrySummary' {} a -> s {registryName = a} :: RegistrySummary)

-- | The ARN of the registry.
registrySummary_registryArn :: Lens.Lens' RegistrySummary (Prelude.Maybe Prelude.Text)
registrySummary_registryArn = Lens.lens (\RegistrySummary' {registryArn} -> registryArn) (\s@RegistrySummary' {} a -> s {registryArn = a} :: RegistrySummary)

-- | Tags associated with the registry.
registrySummary_tags :: Lens.Lens' RegistrySummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
registrySummary_tags = Lens.lens (\RegistrySummary' {tags} -> tags) (\s@RegistrySummary' {} a -> s {tags = a} :: RegistrySummary) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON RegistrySummary where
  parseJSON =
    Core.withObject
      "RegistrySummary"
      ( \x ->
          RegistrySummary'
            Prelude.<$> (x Core..:? "RegistryName")
            Prelude.<*> (x Core..:? "RegistryArn")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable RegistrySummary

instance Prelude.NFData RegistrySummary
