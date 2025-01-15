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
-- Module      : Amazonka.Batch.Types.ShareAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.ShareAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the weights for the fair share identifiers for the fair share
-- policy. Fair share identifiers that aren\'t included have a default
-- weight of @1.0@.
--
-- /See:/ 'newShareAttributes' smart constructor.
data ShareAttributes = ShareAttributes'
  { -- | The weight factor for the fair share identifier. The default value is
    -- 1.0. A lower value has a higher priority for compute resources. For
    -- example, jobs that use a share identifier with a weight factor of 0.125
    -- (1\/8) get 8 times the compute resources of jobs that use a share
    -- identifier with a weight factor of 1.
    --
    -- The smallest supported value is 0.0001, and the largest supported value
    -- is 999.9999.
    weightFactor :: Prelude.Maybe Prelude.Double,
    -- | A fair share identifier or fair share identifier prefix. If the string
    -- ends with an asterisk (*), this entry specifies the weight factor to use
    -- for fair share identifiers that start with that prefix. The list of fair
    -- share identifiers in a fair share policy can\'t overlap. For example,
    -- you can\'t have one that specifies a @shareIdentifier@ of @UserA*@ and
    -- another that specifies a @shareIdentifier@ of @UserA-1@.
    --
    -- There can be no more than 500 fair share identifiers active in a job
    -- queue.
    --
    -- The string is limited to 255 alphanumeric characters, and can be
    -- followed by an asterisk (*).
    shareIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShareAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'weightFactor', 'shareAttributes_weightFactor' - The weight factor for the fair share identifier. The default value is
-- 1.0. A lower value has a higher priority for compute resources. For
-- example, jobs that use a share identifier with a weight factor of 0.125
-- (1\/8) get 8 times the compute resources of jobs that use a share
-- identifier with a weight factor of 1.
--
-- The smallest supported value is 0.0001, and the largest supported value
-- is 999.9999.
--
-- 'shareIdentifier', 'shareAttributes_shareIdentifier' - A fair share identifier or fair share identifier prefix. If the string
-- ends with an asterisk (*), this entry specifies the weight factor to use
-- for fair share identifiers that start with that prefix. The list of fair
-- share identifiers in a fair share policy can\'t overlap. For example,
-- you can\'t have one that specifies a @shareIdentifier@ of @UserA*@ and
-- another that specifies a @shareIdentifier@ of @UserA-1@.
--
-- There can be no more than 500 fair share identifiers active in a job
-- queue.
--
-- The string is limited to 255 alphanumeric characters, and can be
-- followed by an asterisk (*).
newShareAttributes ::
  -- | 'shareIdentifier'
  Prelude.Text ->
  ShareAttributes
newShareAttributes pShareIdentifier_ =
  ShareAttributes'
    { weightFactor = Prelude.Nothing,
      shareIdentifier = pShareIdentifier_
    }

-- | The weight factor for the fair share identifier. The default value is
-- 1.0. A lower value has a higher priority for compute resources. For
-- example, jobs that use a share identifier with a weight factor of 0.125
-- (1\/8) get 8 times the compute resources of jobs that use a share
-- identifier with a weight factor of 1.
--
-- The smallest supported value is 0.0001, and the largest supported value
-- is 999.9999.
shareAttributes_weightFactor :: Lens.Lens' ShareAttributes (Prelude.Maybe Prelude.Double)
shareAttributes_weightFactor = Lens.lens (\ShareAttributes' {weightFactor} -> weightFactor) (\s@ShareAttributes' {} a -> s {weightFactor = a} :: ShareAttributes)

-- | A fair share identifier or fair share identifier prefix. If the string
-- ends with an asterisk (*), this entry specifies the weight factor to use
-- for fair share identifiers that start with that prefix. The list of fair
-- share identifiers in a fair share policy can\'t overlap. For example,
-- you can\'t have one that specifies a @shareIdentifier@ of @UserA*@ and
-- another that specifies a @shareIdentifier@ of @UserA-1@.
--
-- There can be no more than 500 fair share identifiers active in a job
-- queue.
--
-- The string is limited to 255 alphanumeric characters, and can be
-- followed by an asterisk (*).
shareAttributes_shareIdentifier :: Lens.Lens' ShareAttributes Prelude.Text
shareAttributes_shareIdentifier = Lens.lens (\ShareAttributes' {shareIdentifier} -> shareIdentifier) (\s@ShareAttributes' {} a -> s {shareIdentifier = a} :: ShareAttributes)

instance Data.FromJSON ShareAttributes where
  parseJSON =
    Data.withObject
      "ShareAttributes"
      ( \x ->
          ShareAttributes'
            Prelude.<$> (x Data..:? "weightFactor")
            Prelude.<*> (x Data..: "shareIdentifier")
      )

instance Prelude.Hashable ShareAttributes where
  hashWithSalt _salt ShareAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` weightFactor
      `Prelude.hashWithSalt` shareIdentifier

instance Prelude.NFData ShareAttributes where
  rnf ShareAttributes' {..} =
    Prelude.rnf weightFactor `Prelude.seq`
      Prelude.rnf shareIdentifier

instance Data.ToJSON ShareAttributes where
  toJSON ShareAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("weightFactor" Data..=) Prelude.<$> weightFactor,
            Prelude.Just
              ("shareIdentifier" Data..= shareIdentifier)
          ]
      )
