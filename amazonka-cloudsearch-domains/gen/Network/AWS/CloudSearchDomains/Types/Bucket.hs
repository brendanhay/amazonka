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
-- Module      : Network.AWS.CloudSearchDomains.Types.Bucket
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.Bucket where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A container for facet information.
--
-- /See:/ 'newBucket' smart constructor.
data Bucket = Bucket'
  { -- | The facet value being counted.
    value :: Prelude.Maybe Prelude.Text,
    -- | The number of hits that contain the facet value in the specified facet
    -- field.
    count :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Bucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'bucket_value' - The facet value being counted.
--
-- 'count', 'bucket_count' - The number of hits that contain the facet value in the specified facet
-- field.
newBucket ::
  Bucket
newBucket =
  Bucket'
    { value = Prelude.Nothing,
      count = Prelude.Nothing
    }

-- | The facet value being counted.
bucket_value :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_value = Lens.lens (\Bucket' {value} -> value) (\s@Bucket' {} a -> s {value = a} :: Bucket)

-- | The number of hits that contain the facet value in the specified facet
-- field.
bucket_count :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Integer)
bucket_count = Lens.lens (\Bucket' {count} -> count) (\s@Bucket' {} a -> s {count = a} :: Bucket)

instance Prelude.FromJSON Bucket where
  parseJSON =
    Prelude.withObject
      "Bucket"
      ( \x ->
          Bucket'
            Prelude.<$> (x Prelude..:? "value")
            Prelude.<*> (x Prelude..:? "count")
      )

instance Prelude.Hashable Bucket

instance Prelude.NFData Bucket
