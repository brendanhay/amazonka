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
-- Module      : Amazonka.Signer.Types.Destination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.Destination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Signer.Types.S3Destination

-- | Points to an @S3Destination@ object that contains information about your
-- S3 bucket.
--
-- /See:/ 'newDestination' smart constructor.
data Destination = Destination'
  { -- | The @S3Destination@ object.
    s3 :: Prelude.Maybe S3Destination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3', 'destination_s3' - The @S3Destination@ object.
newDestination ::
  Destination
newDestination = Destination' {s3 = Prelude.Nothing}

-- | The @S3Destination@ object.
destination_s3 :: Lens.Lens' Destination (Prelude.Maybe S3Destination)
destination_s3 = Lens.lens (\Destination' {s3} -> s3) (\s@Destination' {} a -> s {s3 = a} :: Destination)

instance Prelude.Hashable Destination where
  hashWithSalt _salt Destination' {..} =
    _salt `Prelude.hashWithSalt` s3

instance Prelude.NFData Destination where
  rnf Destination' {..} = Prelude.rnf s3

instance Core.ToJSON Destination where
  toJSON Destination' {..} =
    Core.object
      (Prelude.catMaybes [("s3" Core..=) Prelude.<$> s3])
