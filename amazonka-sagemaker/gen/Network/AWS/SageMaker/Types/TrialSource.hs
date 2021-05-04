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
-- Module      : Network.AWS.SageMaker.Types.TrialSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialSource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The source of the trial.
--
-- /See:/ 'newTrialSource' smart constructor.
data TrialSource = TrialSource'
  { -- | The source job type.
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the source.
    sourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TrialSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceType', 'trialSource_sourceType' - The source job type.
--
-- 'sourceArn', 'trialSource_sourceArn' - The Amazon Resource Name (ARN) of the source.
newTrialSource ::
  -- | 'sourceArn'
  Prelude.Text ->
  TrialSource
newTrialSource pSourceArn_ =
  TrialSource'
    { sourceType = Prelude.Nothing,
      sourceArn = pSourceArn_
    }

-- | The source job type.
trialSource_sourceType :: Lens.Lens' TrialSource (Prelude.Maybe Prelude.Text)
trialSource_sourceType = Lens.lens (\TrialSource' {sourceType} -> sourceType) (\s@TrialSource' {} a -> s {sourceType = a} :: TrialSource)

-- | The Amazon Resource Name (ARN) of the source.
trialSource_sourceArn :: Lens.Lens' TrialSource Prelude.Text
trialSource_sourceArn = Lens.lens (\TrialSource' {sourceArn} -> sourceArn) (\s@TrialSource' {} a -> s {sourceArn = a} :: TrialSource)

instance Prelude.FromJSON TrialSource where
  parseJSON =
    Prelude.withObject
      "TrialSource"
      ( \x ->
          TrialSource'
            Prelude.<$> (x Prelude..:? "SourceType")
            Prelude.<*> (x Prelude..: "SourceArn")
      )

instance Prelude.Hashable TrialSource

instance Prelude.NFData TrialSource
