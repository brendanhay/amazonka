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
-- Module      : Amazonka.XRay.Types.SamplingRuleRecord
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.SamplingRuleRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.SamplingRule

-- | A
-- <https://docs.aws.amazon.com/xray/latest/api/API_SamplingRule.html SamplingRule>
-- and its metadata.
--
-- /See:/ 'newSamplingRuleRecord' smart constructor.
data SamplingRuleRecord = SamplingRuleRecord'
  { -- | When the rule was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | When the rule was last modified.
    modifiedAt :: Prelude.Maybe Data.POSIX,
    -- | The sampling rule.
    samplingRule :: Prelude.Maybe SamplingRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SamplingRuleRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'samplingRuleRecord_createdAt' - When the rule was created.
--
-- 'modifiedAt', 'samplingRuleRecord_modifiedAt' - When the rule was last modified.
--
-- 'samplingRule', 'samplingRuleRecord_samplingRule' - The sampling rule.
newSamplingRuleRecord ::
  SamplingRuleRecord
newSamplingRuleRecord =
  SamplingRuleRecord'
    { createdAt = Prelude.Nothing,
      modifiedAt = Prelude.Nothing,
      samplingRule = Prelude.Nothing
    }

-- | When the rule was created.
samplingRuleRecord_createdAt :: Lens.Lens' SamplingRuleRecord (Prelude.Maybe Prelude.UTCTime)
samplingRuleRecord_createdAt = Lens.lens (\SamplingRuleRecord' {createdAt} -> createdAt) (\s@SamplingRuleRecord' {} a -> s {createdAt = a} :: SamplingRuleRecord) Prelude.. Lens.mapping Data._Time

-- | When the rule was last modified.
samplingRuleRecord_modifiedAt :: Lens.Lens' SamplingRuleRecord (Prelude.Maybe Prelude.UTCTime)
samplingRuleRecord_modifiedAt = Lens.lens (\SamplingRuleRecord' {modifiedAt} -> modifiedAt) (\s@SamplingRuleRecord' {} a -> s {modifiedAt = a} :: SamplingRuleRecord) Prelude.. Lens.mapping Data._Time

-- | The sampling rule.
samplingRuleRecord_samplingRule :: Lens.Lens' SamplingRuleRecord (Prelude.Maybe SamplingRule)
samplingRuleRecord_samplingRule = Lens.lens (\SamplingRuleRecord' {samplingRule} -> samplingRule) (\s@SamplingRuleRecord' {} a -> s {samplingRule = a} :: SamplingRuleRecord)

instance Data.FromJSON SamplingRuleRecord where
  parseJSON =
    Data.withObject
      "SamplingRuleRecord"
      ( \x ->
          SamplingRuleRecord'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "ModifiedAt")
            Prelude.<*> (x Data..:? "SamplingRule")
      )

instance Prelude.Hashable SamplingRuleRecord where
  hashWithSalt _salt SamplingRuleRecord' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` modifiedAt
      `Prelude.hashWithSalt` samplingRule

instance Prelude.NFData SamplingRuleRecord where
  rnf SamplingRuleRecord' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf modifiedAt
      `Prelude.seq` Prelude.rnf samplingRule
