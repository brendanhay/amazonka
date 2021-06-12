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
-- Module      : Network.AWS.EC2.Types.Storage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Storage where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.S3Storage
import qualified Network.AWS.Lens as Lens

-- | Describes the storage location for an instance store-backed AMI.
--
-- /See:/ 'newStorage' smart constructor.
data Storage = Storage'
  { -- | An Amazon S3 storage location.
    s3 :: Core.Maybe S3Storage
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Storage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3', 'storage_s3' - An Amazon S3 storage location.
newStorage ::
  Storage
newStorage = Storage' {s3 = Core.Nothing}

-- | An Amazon S3 storage location.
storage_s3 :: Lens.Lens' Storage (Core.Maybe S3Storage)
storage_s3 = Lens.lens (\Storage' {s3} -> s3) (\s@Storage' {} a -> s {s3 = a} :: Storage)

instance Core.FromXML Storage where
  parseXML x = Storage' Core.<$> (x Core..@? "S3")

instance Core.Hashable Storage

instance Core.NFData Storage

instance Core.ToQuery Storage where
  toQuery Storage' {..} = Core.mconcat ["S3" Core.=: s3]
