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
-- Module      : Amazonka.DataBrew.Types.Metadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.Metadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains additional resource information needed for specific datasets.
--
-- /See:/ 'newMetadata' smart constructor.
data Metadata = Metadata'
  { -- | The Amazon Resource Name (ARN) associated with the dataset. Currently,
    -- DataBrew only supports ARNs from Amazon AppFlow.
    sourceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Metadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceArn', 'metadata_sourceArn' - The Amazon Resource Name (ARN) associated with the dataset. Currently,
-- DataBrew only supports ARNs from Amazon AppFlow.
newMetadata ::
  Metadata
newMetadata = Metadata' {sourceArn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) associated with the dataset. Currently,
-- DataBrew only supports ARNs from Amazon AppFlow.
metadata_sourceArn :: Lens.Lens' Metadata (Prelude.Maybe Prelude.Text)
metadata_sourceArn = Lens.lens (\Metadata' {sourceArn} -> sourceArn) (\s@Metadata' {} a -> s {sourceArn = a} :: Metadata)

instance Data.FromJSON Metadata where
  parseJSON =
    Data.withObject
      "Metadata"
      ( \x ->
          Metadata' Prelude.<$> (x Data..:? "SourceArn")
      )

instance Prelude.Hashable Metadata where
  hashWithSalt _salt Metadata' {..} =
    _salt `Prelude.hashWithSalt` sourceArn

instance Prelude.NFData Metadata where
  rnf Metadata' {..} = Prelude.rnf sourceArn

instance Data.ToJSON Metadata where
  toJSON Metadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [("SourceArn" Data..=) Prelude.<$> sourceArn]
      )
