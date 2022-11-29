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
-- Module      : Amazonka.SageMaker.Types.TrialComponentSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrialComponentSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Resource Name (ARN) and job type of the source of a trial
-- component.
--
-- /See:/ 'newTrialComponentSource' smart constructor.
data TrialComponentSource = TrialComponentSource'
  { -- | The source job type.
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | The source ARN.
    sourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrialComponentSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceType', 'trialComponentSource_sourceType' - The source job type.
--
-- 'sourceArn', 'trialComponentSource_sourceArn' - The source ARN.
newTrialComponentSource ::
  -- | 'sourceArn'
  Prelude.Text ->
  TrialComponentSource
newTrialComponentSource pSourceArn_ =
  TrialComponentSource'
    { sourceType = Prelude.Nothing,
      sourceArn = pSourceArn_
    }

-- | The source job type.
trialComponentSource_sourceType :: Lens.Lens' TrialComponentSource (Prelude.Maybe Prelude.Text)
trialComponentSource_sourceType = Lens.lens (\TrialComponentSource' {sourceType} -> sourceType) (\s@TrialComponentSource' {} a -> s {sourceType = a} :: TrialComponentSource)

-- | The source ARN.
trialComponentSource_sourceArn :: Lens.Lens' TrialComponentSource Prelude.Text
trialComponentSource_sourceArn = Lens.lens (\TrialComponentSource' {sourceArn} -> sourceArn) (\s@TrialComponentSource' {} a -> s {sourceArn = a} :: TrialComponentSource)

instance Core.FromJSON TrialComponentSource where
  parseJSON =
    Core.withObject
      "TrialComponentSource"
      ( \x ->
          TrialComponentSource'
            Prelude.<$> (x Core..:? "SourceType")
            Prelude.<*> (x Core..: "SourceArn")
      )

instance Prelude.Hashable TrialComponentSource where
  hashWithSalt _salt TrialComponentSource' {..} =
    _salt `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` sourceArn

instance Prelude.NFData TrialComponentSource where
  rnf TrialComponentSource' {..} =
    Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf sourceArn
