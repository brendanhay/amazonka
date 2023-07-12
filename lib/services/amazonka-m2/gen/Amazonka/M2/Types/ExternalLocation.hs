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
-- Module      : Amazonka.M2.Types.ExternalLocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.ExternalLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines an external storage location.
--
-- /See:/ 'newExternalLocation' smart constructor.
data ExternalLocation = ExternalLocation'
  { -- | The URI of the Amazon S3 bucket.
    s3Location :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExternalLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Location', 'externalLocation_s3Location' - The URI of the Amazon S3 bucket.
newExternalLocation ::
  ExternalLocation
newExternalLocation =
  ExternalLocation' {s3Location = Prelude.Nothing}

-- | The URI of the Amazon S3 bucket.
externalLocation_s3Location :: Lens.Lens' ExternalLocation (Prelude.Maybe Prelude.Text)
externalLocation_s3Location = Lens.lens (\ExternalLocation' {s3Location} -> s3Location) (\s@ExternalLocation' {} a -> s {s3Location = a} :: ExternalLocation)

instance Prelude.Hashable ExternalLocation where
  hashWithSalt _salt ExternalLocation' {..} =
    _salt `Prelude.hashWithSalt` s3Location

instance Prelude.NFData ExternalLocation where
  rnf ExternalLocation' {..} = Prelude.rnf s3Location

instance Data.ToJSON ExternalLocation where
  toJSON ExternalLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [("s3Location" Data..=) Prelude.<$> s3Location]
      )
