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
-- Module      : Amazonka.S3.Types.GlacierJobParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.GlacierJobParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.Tier

-- | Container for S3 Glacier job parameters.
--
-- /See:/ 'newGlacierJobParameters' smart constructor.
data GlacierJobParameters = GlacierJobParameters'
  { -- | Retrieval tier at which the restore will be processed.
    tier :: Tier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlacierJobParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tier', 'glacierJobParameters_tier' - Retrieval tier at which the restore will be processed.
newGlacierJobParameters ::
  -- | 'tier'
  Tier ->
  GlacierJobParameters
newGlacierJobParameters pTier_ =
  GlacierJobParameters' {tier = pTier_}

-- | Retrieval tier at which the restore will be processed.
glacierJobParameters_tier :: Lens.Lens' GlacierJobParameters Tier
glacierJobParameters_tier = Lens.lens (\GlacierJobParameters' {tier} -> tier) (\s@GlacierJobParameters' {} a -> s {tier = a} :: GlacierJobParameters)

instance Prelude.Hashable GlacierJobParameters where
  hashWithSalt _salt GlacierJobParameters' {..} =
    _salt `Prelude.hashWithSalt` tier

instance Prelude.NFData GlacierJobParameters where
  rnf GlacierJobParameters' {..} = Prelude.rnf tier

instance Data.ToXML GlacierJobParameters where
  toXML GlacierJobParameters' {..} =
    Prelude.mconcat ["Tier" Data.@= tier]
