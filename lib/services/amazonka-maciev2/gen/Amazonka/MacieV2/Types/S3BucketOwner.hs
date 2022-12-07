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
-- Module      : Amazonka.MacieV2.Types.S3BucketOwner
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.S3BucketOwner where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the Amazon Web Services account that owns an
-- S3 bucket.
--
-- /See:/ 'newS3BucketOwner' smart constructor.
data S3BucketOwner = S3BucketOwner'
  { -- | The display name of the account that owns the bucket.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The canonical user ID for the account that owns the bucket.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3BucketOwner' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 's3BucketOwner_displayName' - The display name of the account that owns the bucket.
--
-- 'id', 's3BucketOwner_id' - The canonical user ID for the account that owns the bucket.
newS3BucketOwner ::
  S3BucketOwner
newS3BucketOwner =
  S3BucketOwner'
    { displayName = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The display name of the account that owns the bucket.
s3BucketOwner_displayName :: Lens.Lens' S3BucketOwner (Prelude.Maybe Prelude.Text)
s3BucketOwner_displayName = Lens.lens (\S3BucketOwner' {displayName} -> displayName) (\s@S3BucketOwner' {} a -> s {displayName = a} :: S3BucketOwner)

-- | The canonical user ID for the account that owns the bucket.
s3BucketOwner_id :: Lens.Lens' S3BucketOwner (Prelude.Maybe Prelude.Text)
s3BucketOwner_id = Lens.lens (\S3BucketOwner' {id} -> id) (\s@S3BucketOwner' {} a -> s {id = a} :: S3BucketOwner)

instance Data.FromJSON S3BucketOwner where
  parseJSON =
    Data.withObject
      "S3BucketOwner"
      ( \x ->
          S3BucketOwner'
            Prelude.<$> (x Data..:? "displayName")
            Prelude.<*> (x Data..:? "id")
      )

instance Prelude.Hashable S3BucketOwner where
  hashWithSalt _salt S3BucketOwner' {..} =
    _salt `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` id

instance Prelude.NFData S3BucketOwner where
  rnf S3BucketOwner' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf id
