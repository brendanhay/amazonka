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
-- Module      : Amazonka.SSM.Types.OutputSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.OutputSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the source where the association execution details are
-- stored.
--
-- /See:/ 'newOutputSource' smart constructor.
data OutputSource = OutputSource'
  { -- | The ID of the output source, for example the URL of an S3 bucket.
    outputSourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of source where the association execution details are stored,
    -- for example, Amazon S3.
    outputSourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputSourceId', 'outputSource_outputSourceId' - The ID of the output source, for example the URL of an S3 bucket.
--
-- 'outputSourceType', 'outputSource_outputSourceType' - The type of source where the association execution details are stored,
-- for example, Amazon S3.
newOutputSource ::
  OutputSource
newOutputSource =
  OutputSource'
    { outputSourceId = Prelude.Nothing,
      outputSourceType = Prelude.Nothing
    }

-- | The ID of the output source, for example the URL of an S3 bucket.
outputSource_outputSourceId :: Lens.Lens' OutputSource (Prelude.Maybe Prelude.Text)
outputSource_outputSourceId = Lens.lens (\OutputSource' {outputSourceId} -> outputSourceId) (\s@OutputSource' {} a -> s {outputSourceId = a} :: OutputSource)

-- | The type of source where the association execution details are stored,
-- for example, Amazon S3.
outputSource_outputSourceType :: Lens.Lens' OutputSource (Prelude.Maybe Prelude.Text)
outputSource_outputSourceType = Lens.lens (\OutputSource' {outputSourceType} -> outputSourceType) (\s@OutputSource' {} a -> s {outputSourceType = a} :: OutputSource)

instance Data.FromJSON OutputSource where
  parseJSON =
    Data.withObject
      "OutputSource"
      ( \x ->
          OutputSource'
            Prelude.<$> (x Data..:? "OutputSourceId")
            Prelude.<*> (x Data..:? "OutputSourceType")
      )

instance Prelude.Hashable OutputSource where
  hashWithSalt _salt OutputSource' {..} =
    _salt
      `Prelude.hashWithSalt` outputSourceId
      `Prelude.hashWithSalt` outputSourceType

instance Prelude.NFData OutputSource where
  rnf OutputSource' {..} =
    Prelude.rnf outputSourceId
      `Prelude.seq` Prelude.rnf outputSourceType
