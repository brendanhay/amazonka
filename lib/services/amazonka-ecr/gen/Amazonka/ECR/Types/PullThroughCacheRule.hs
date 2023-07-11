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
-- Module      : Amazonka.ECR.Types.PullThroughCacheRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.PullThroughCacheRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of a pull through cache rule.
--
-- /See:/ 'newPullThroughCacheRule' smart constructor.
data PullThroughCacheRule = PullThroughCacheRule'
  { -- | The date and time the pull through cache was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The Amazon ECR repository prefix associated with the pull through cache
    -- rule.
    ecrRepositoryPrefix :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID associated with the registry the pull
    -- through cache rule is associated with.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The upstream registry URL associated with the pull through cache rule.
    upstreamRegistryUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PullThroughCacheRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'pullThroughCacheRule_createdAt' - The date and time the pull through cache was created.
--
-- 'ecrRepositoryPrefix', 'pullThroughCacheRule_ecrRepositoryPrefix' - The Amazon ECR repository prefix associated with the pull through cache
-- rule.
--
-- 'registryId', 'pullThroughCacheRule_registryId' - The Amazon Web Services account ID associated with the registry the pull
-- through cache rule is associated with.
--
-- 'upstreamRegistryUrl', 'pullThroughCacheRule_upstreamRegistryUrl' - The upstream registry URL associated with the pull through cache rule.
newPullThroughCacheRule ::
  PullThroughCacheRule
newPullThroughCacheRule =
  PullThroughCacheRule'
    { createdAt = Prelude.Nothing,
      ecrRepositoryPrefix = Prelude.Nothing,
      registryId = Prelude.Nothing,
      upstreamRegistryUrl = Prelude.Nothing
    }

-- | The date and time the pull through cache was created.
pullThroughCacheRule_createdAt :: Lens.Lens' PullThroughCacheRule (Prelude.Maybe Prelude.UTCTime)
pullThroughCacheRule_createdAt = Lens.lens (\PullThroughCacheRule' {createdAt} -> createdAt) (\s@PullThroughCacheRule' {} a -> s {createdAt = a} :: PullThroughCacheRule) Prelude.. Lens.mapping Data._Time

-- | The Amazon ECR repository prefix associated with the pull through cache
-- rule.
pullThroughCacheRule_ecrRepositoryPrefix :: Lens.Lens' PullThroughCacheRule (Prelude.Maybe Prelude.Text)
pullThroughCacheRule_ecrRepositoryPrefix = Lens.lens (\PullThroughCacheRule' {ecrRepositoryPrefix} -> ecrRepositoryPrefix) (\s@PullThroughCacheRule' {} a -> s {ecrRepositoryPrefix = a} :: PullThroughCacheRule)

-- | The Amazon Web Services account ID associated with the registry the pull
-- through cache rule is associated with.
pullThroughCacheRule_registryId :: Lens.Lens' PullThroughCacheRule (Prelude.Maybe Prelude.Text)
pullThroughCacheRule_registryId = Lens.lens (\PullThroughCacheRule' {registryId} -> registryId) (\s@PullThroughCacheRule' {} a -> s {registryId = a} :: PullThroughCacheRule)

-- | The upstream registry URL associated with the pull through cache rule.
pullThroughCacheRule_upstreamRegistryUrl :: Lens.Lens' PullThroughCacheRule (Prelude.Maybe Prelude.Text)
pullThroughCacheRule_upstreamRegistryUrl = Lens.lens (\PullThroughCacheRule' {upstreamRegistryUrl} -> upstreamRegistryUrl) (\s@PullThroughCacheRule' {} a -> s {upstreamRegistryUrl = a} :: PullThroughCacheRule)

instance Data.FromJSON PullThroughCacheRule where
  parseJSON =
    Data.withObject
      "PullThroughCacheRule"
      ( \x ->
          PullThroughCacheRule'
            Prelude.<$> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "ecrRepositoryPrefix")
            Prelude.<*> (x Data..:? "registryId")
            Prelude.<*> (x Data..:? "upstreamRegistryUrl")
      )

instance Prelude.Hashable PullThroughCacheRule where
  hashWithSalt _salt PullThroughCacheRule' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` ecrRepositoryPrefix
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` upstreamRegistryUrl

instance Prelude.NFData PullThroughCacheRule where
  rnf PullThroughCacheRule' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf ecrRepositoryPrefix
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf upstreamRegistryUrl
