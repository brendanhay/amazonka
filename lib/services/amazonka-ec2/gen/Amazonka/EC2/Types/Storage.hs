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
-- Module      : Amazonka.EC2.Types.Storage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Storage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.S3Storage
import qualified Amazonka.Prelude as Prelude

-- | Describes the storage location for an instance store-backed AMI.
--
-- /See:/ 'newStorage' smart constructor.
data Storage = Storage'
  { -- | An Amazon S3 storage location.
    s3 :: Prelude.Maybe S3Storage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromXML Storage where
  parseXML x = Storage' Prelude.<$> (x Data..@? "S3")

instance Prelude.Hashable Storage where
  hashWithSalt _salt Storage' {..} =
    _salt `Prelude.hashWithSalt` s3

instance Prelude.NFData Storage where
  rnf Storage' {..} = Prelude.rnf s3

instance Data.ToQuery Storage where
  toQuery Storage' {..} =
    Prelude.mconcat ["S3" Data.=: s3]
