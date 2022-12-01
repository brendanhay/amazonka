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
-- Module      : Amazonka.Kafka.Types.Unauthenticated
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.Unauthenticated where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newUnauthenticated' smart constructor.
data Unauthenticated = Unauthenticated'
  { -- | Specifies whether you want to turn on or turn off unauthenticated
    -- traffic to your cluster.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Unauthenticated' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'unauthenticated_enabled' - Specifies whether you want to turn on or turn off unauthenticated
-- traffic to your cluster.
newUnauthenticated ::
  Unauthenticated
newUnauthenticated =
  Unauthenticated' {enabled = Prelude.Nothing}

-- | Specifies whether you want to turn on or turn off unauthenticated
-- traffic to your cluster.
unauthenticated_enabled :: Lens.Lens' Unauthenticated (Prelude.Maybe Prelude.Bool)
unauthenticated_enabled = Lens.lens (\Unauthenticated' {enabled} -> enabled) (\s@Unauthenticated' {} a -> s {enabled = a} :: Unauthenticated)

instance Core.FromJSON Unauthenticated where
  parseJSON =
    Core.withObject
      "Unauthenticated"
      ( \x ->
          Unauthenticated' Prelude.<$> (x Core..:? "enabled")
      )

instance Prelude.Hashable Unauthenticated where
  hashWithSalt _salt Unauthenticated' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData Unauthenticated where
  rnf Unauthenticated' {..} = Prelude.rnf enabled

instance Core.ToJSON Unauthenticated where
  toJSON Unauthenticated' {..} =
    Core.object
      ( Prelude.catMaybes
          [("enabled" Core..=) Prelude.<$> enabled]
      )
