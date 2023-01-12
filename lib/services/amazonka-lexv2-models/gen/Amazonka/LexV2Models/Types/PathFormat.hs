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
-- Module      : Amazonka.LexV2Models.Types.PathFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.PathFormat where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The object that contains a path format that will be applied when Amazon
-- Lex reads the transcript file in the bucket you provide. Specify this
-- object if you only want Lex to read a subset of files in your Amazon S3
-- bucket.
--
-- /See:/ 'newPathFormat' smart constructor.
data PathFormat = PathFormat'
  { -- | A list of Amazon S3 prefixes that points to sub-folders in the Amazon S3
    -- bucket. Specify this list if you only want Lex to read the files under
    -- this set of sub-folders.
    objectPrefixes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PathFormat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectPrefixes', 'pathFormat_objectPrefixes' - A list of Amazon S3 prefixes that points to sub-folders in the Amazon S3
-- bucket. Specify this list if you only want Lex to read the files under
-- this set of sub-folders.
newPathFormat ::
  PathFormat
newPathFormat =
  PathFormat' {objectPrefixes = Prelude.Nothing}

-- | A list of Amazon S3 prefixes that points to sub-folders in the Amazon S3
-- bucket. Specify this list if you only want Lex to read the files under
-- this set of sub-folders.
pathFormat_objectPrefixes :: Lens.Lens' PathFormat (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
pathFormat_objectPrefixes = Lens.lens (\PathFormat' {objectPrefixes} -> objectPrefixes) (\s@PathFormat' {} a -> s {objectPrefixes = a} :: PathFormat) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PathFormat where
  parseJSON =
    Data.withObject
      "PathFormat"
      ( \x ->
          PathFormat'
            Prelude.<$> (x Data..:? "objectPrefixes")
      )

instance Prelude.Hashable PathFormat where
  hashWithSalt _salt PathFormat' {..} =
    _salt `Prelude.hashWithSalt` objectPrefixes

instance Prelude.NFData PathFormat where
  rnf PathFormat' {..} = Prelude.rnf objectPrefixes

instance Data.ToJSON PathFormat where
  toJSON PathFormat' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("objectPrefixes" Data..=)
              Prelude.<$> objectPrefixes
          ]
      )
