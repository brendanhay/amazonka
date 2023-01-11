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
-- Module      : Amazonka.Signer.Types.Source
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.Source where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Signer.Types.S3Source

-- | An @S3Source@ object that contains information about the S3 bucket where
-- you saved your unsigned code.
--
-- /See:/ 'newSource' smart constructor.
data Source = Source'
  { -- | The @S3Source@ object.
    s3 :: Prelude.Maybe S3Source
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Source' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3', 'source_s3' - The @S3Source@ object.
newSource ::
  Source
newSource = Source' {s3 = Prelude.Nothing}

-- | The @S3Source@ object.
source_s3 :: Lens.Lens' Source (Prelude.Maybe S3Source)
source_s3 = Lens.lens (\Source' {s3} -> s3) (\s@Source' {} a -> s {s3 = a} :: Source)

instance Data.FromJSON Source where
  parseJSON =
    Data.withObject
      "Source"
      (\x -> Source' Prelude.<$> (x Data..:? "s3"))

instance Prelude.Hashable Source where
  hashWithSalt _salt Source' {..} =
    _salt `Prelude.hashWithSalt` s3

instance Prelude.NFData Source where
  rnf Source' {..} = Prelude.rnf s3

instance Data.ToJSON Source where
  toJSON Source' {..} =
    Data.object
      (Prelude.catMaybes [("s3" Data..=) Prelude.<$> s3])
