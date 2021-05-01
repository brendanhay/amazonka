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
-- Module      : Network.AWS.EC2.Types.Storage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Storage where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.S3Storage
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the storage location for an instance store-backed AMI.
--
-- /See:/ 'newStorage' smart constructor.
data Storage = Storage'
  { -- | An Amazon S3 storage location.
    s3 :: Prelude.Maybe S3Storage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
newStorage = Storage' {s3 = Prelude.Nothing}

-- | An Amazon S3 storage location.
storage_s3 :: Lens.Lens' Storage (Prelude.Maybe S3Storage)
storage_s3 = Lens.lens (\Storage' {s3} -> s3) (\s@Storage' {} a -> s {s3 = a} :: Storage)

instance Prelude.FromXML Storage where
  parseXML x =
    Storage' Prelude.<$> (x Prelude..@? "S3")

instance Prelude.Hashable Storage

instance Prelude.NFData Storage

instance Prelude.ToQuery Storage where
  toQuery Storage' {..} =
    Prelude.mconcat ["S3" Prelude.=: s3]
