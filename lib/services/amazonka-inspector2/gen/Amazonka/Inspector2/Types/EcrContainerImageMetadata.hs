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
-- Module      : Amazonka.Inspector2.Types.EcrContainerImageMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.EcrContainerImageMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information on the Amazon ECR image metadata associated with a finding.
--
-- /See:/ 'newEcrContainerImageMetadata' smart constructor.
data EcrContainerImageMetadata = EcrContainerImageMetadata'
  { -- | Tags associated with the Amazon ECR image metadata.
    tags :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EcrContainerImageMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'ecrContainerImageMetadata_tags' - Tags associated with the Amazon ECR image metadata.
newEcrContainerImageMetadata ::
  EcrContainerImageMetadata
newEcrContainerImageMetadata =
  EcrContainerImageMetadata' {tags = Prelude.Nothing}

-- | Tags associated with the Amazon ECR image metadata.
ecrContainerImageMetadata_tags :: Lens.Lens' EcrContainerImageMetadata (Prelude.Maybe [Prelude.Text])
ecrContainerImageMetadata_tags = Lens.lens (\EcrContainerImageMetadata' {tags} -> tags) (\s@EcrContainerImageMetadata' {} a -> s {tags = a} :: EcrContainerImageMetadata) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EcrContainerImageMetadata where
  parseJSON =
    Data.withObject
      "EcrContainerImageMetadata"
      ( \x ->
          EcrContainerImageMetadata'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable EcrContainerImageMetadata where
  hashWithSalt _salt EcrContainerImageMetadata' {..} =
    _salt `Prelude.hashWithSalt` tags

instance Prelude.NFData EcrContainerImageMetadata where
  rnf EcrContainerImageMetadata' {..} = Prelude.rnf tags
