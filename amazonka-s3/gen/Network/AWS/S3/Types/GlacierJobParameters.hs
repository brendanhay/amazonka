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
-- Module      : Network.AWS.S3.Types.GlacierJobParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.GlacierJobParameters where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Tier

-- | Container for S3 Glacier job parameters.
--
-- /See:/ 'newGlacierJobParameters' smart constructor.
data GlacierJobParameters = GlacierJobParameters'
  { -- | Retrieval tier at which the restore will be processed.
    tier :: Tier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable GlacierJobParameters

instance Prelude.NFData GlacierJobParameters

instance Prelude.ToXML GlacierJobParameters where
  toXML GlacierJobParameters' {..} =
    Prelude.mconcat ["Tier" Prelude.@= tier]
