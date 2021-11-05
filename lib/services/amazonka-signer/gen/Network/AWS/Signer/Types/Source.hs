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
-- Module      : Network.AWS.Signer.Types.Source
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Signer.Types.Source where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Signer.Types.S3Source

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

instance Core.FromJSON Source where
  parseJSON =
    Core.withObject
      "Source"
      (\x -> Source' Prelude.<$> (x Core..:? "s3"))

instance Prelude.Hashable Source

instance Prelude.NFData Source

instance Core.ToJSON Source where
  toJSON Source' {..} =
    Core.object
      (Prelude.catMaybes [("s3" Core..=) Prelude.<$> s3])
