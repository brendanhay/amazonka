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
-- Module      : Amazonka.MacieV2.Types.MatchingResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.MatchingResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.MatchingBucket
import qualified Amazonka.Prelude as Prelude

-- | Provides statistical data and other information about an Amazon Web
-- Services resource that Amazon Macie monitors and analyzes for your
-- account.
--
-- /See:/ 'newMatchingResource' smart constructor.
data MatchingResource = MatchingResource'
  { -- | The details of an S3 bucket that Amazon Macie monitors and analyzes.
    matchingBucket :: Prelude.Maybe MatchingBucket
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MatchingResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'matchingBucket', 'matchingResource_matchingBucket' - The details of an S3 bucket that Amazon Macie monitors and analyzes.
newMatchingResource ::
  MatchingResource
newMatchingResource =
  MatchingResource' {matchingBucket = Prelude.Nothing}

-- | The details of an S3 bucket that Amazon Macie monitors and analyzes.
matchingResource_matchingBucket :: Lens.Lens' MatchingResource (Prelude.Maybe MatchingBucket)
matchingResource_matchingBucket = Lens.lens (\MatchingResource' {matchingBucket} -> matchingBucket) (\s@MatchingResource' {} a -> s {matchingBucket = a} :: MatchingResource)

instance Data.FromJSON MatchingResource where
  parseJSON =
    Data.withObject
      "MatchingResource"
      ( \x ->
          MatchingResource'
            Prelude.<$> (x Data..:? "matchingBucket")
      )

instance Prelude.Hashable MatchingResource where
  hashWithSalt _salt MatchingResource' {..} =
    _salt `Prelude.hashWithSalt` matchingBucket

instance Prelude.NFData MatchingResource where
  rnf MatchingResource' {..} =
    Prelude.rnf matchingBucket
