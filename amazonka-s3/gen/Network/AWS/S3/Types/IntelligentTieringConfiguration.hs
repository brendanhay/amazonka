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
-- Module      : Network.AWS.S3.Types.IntelligentTieringConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.IntelligentTieringConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.IntelligentTieringFilter
import Network.AWS.S3.Types.IntelligentTieringStatus
import Network.AWS.S3.Types.Tiering

-- | Specifies the S3 Intelligent-Tiering configuration for an Amazon S3
-- bucket.
--
-- For information about the S3 Intelligent-Tiering storage class, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects>.
--
-- /See:/ 'newIntelligentTieringConfiguration' smart constructor.
data IntelligentTieringConfiguration = IntelligentTieringConfiguration'
  { -- | Specifies a bucket filter. The configuration only includes objects that
    -- meet the filter\'s criteria.
    filter' :: Core.Maybe IntelligentTieringFilter,
    -- | The ID used to identify the S3 Intelligent-Tiering configuration.
    id :: Core.Text,
    -- | Specifies the status of the configuration.
    status :: IntelligentTieringStatus,
    -- | Specifies the S3 Intelligent-Tiering storage class tier of the
    -- configuration.
    tierings :: [Tiering]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IntelligentTieringConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'intelligentTieringConfiguration_filter' - Specifies a bucket filter. The configuration only includes objects that
-- meet the filter\'s criteria.
--
-- 'id', 'intelligentTieringConfiguration_id' - The ID used to identify the S3 Intelligent-Tiering configuration.
--
-- 'status', 'intelligentTieringConfiguration_status' - Specifies the status of the configuration.
--
-- 'tierings', 'intelligentTieringConfiguration_tierings' - Specifies the S3 Intelligent-Tiering storage class tier of the
-- configuration.
newIntelligentTieringConfiguration ::
  -- | 'id'
  Core.Text ->
  -- | 'status'
  IntelligentTieringStatus ->
  IntelligentTieringConfiguration
newIntelligentTieringConfiguration pId_ pStatus_ =
  IntelligentTieringConfiguration'
    { filter' =
        Core.Nothing,
      id = pId_,
      status = pStatus_,
      tierings = Core.mempty
    }

-- | Specifies a bucket filter. The configuration only includes objects that
-- meet the filter\'s criteria.
intelligentTieringConfiguration_filter :: Lens.Lens' IntelligentTieringConfiguration (Core.Maybe IntelligentTieringFilter)
intelligentTieringConfiguration_filter = Lens.lens (\IntelligentTieringConfiguration' {filter'} -> filter') (\s@IntelligentTieringConfiguration' {} a -> s {filter' = a} :: IntelligentTieringConfiguration)

-- | The ID used to identify the S3 Intelligent-Tiering configuration.
intelligentTieringConfiguration_id :: Lens.Lens' IntelligentTieringConfiguration Core.Text
intelligentTieringConfiguration_id = Lens.lens (\IntelligentTieringConfiguration' {id} -> id) (\s@IntelligentTieringConfiguration' {} a -> s {id = a} :: IntelligentTieringConfiguration)

-- | Specifies the status of the configuration.
intelligentTieringConfiguration_status :: Lens.Lens' IntelligentTieringConfiguration IntelligentTieringStatus
intelligentTieringConfiguration_status = Lens.lens (\IntelligentTieringConfiguration' {status} -> status) (\s@IntelligentTieringConfiguration' {} a -> s {status = a} :: IntelligentTieringConfiguration)

-- | Specifies the S3 Intelligent-Tiering storage class tier of the
-- configuration.
intelligentTieringConfiguration_tierings :: Lens.Lens' IntelligentTieringConfiguration [Tiering]
intelligentTieringConfiguration_tierings = Lens.lens (\IntelligentTieringConfiguration' {tierings} -> tierings) (\s@IntelligentTieringConfiguration' {} a -> s {tierings = a} :: IntelligentTieringConfiguration) Core.. Lens._Coerce

instance Core.FromXML IntelligentTieringConfiguration where
  parseXML x =
    IntelligentTieringConfiguration'
      Core.<$> (x Core..@? "Filter")
      Core.<*> (x Core..@ "Id")
      Core.<*> (x Core..@ "Status")
      Core.<*> (Core.parseXMLList "Tiering" x)

instance
  Core.Hashable
    IntelligentTieringConfiguration

instance Core.NFData IntelligentTieringConfiguration

instance Core.ToXML IntelligentTieringConfiguration where
  toXML IntelligentTieringConfiguration' {..} =
    Core.mconcat
      [ "Filter" Core.@= filter',
        "Id" Core.@= id,
        "Status" Core.@= status,
        Core.toXMLList "Tiering" tierings
      ]
