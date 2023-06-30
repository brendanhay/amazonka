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
-- Module      : Amazonka.MacieV2.Types.AccessControlList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.AccessControlList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the permissions settings of the bucket-level
-- access control list (ACL) for an S3 bucket.
--
-- /See:/ 'newAccessControlList' smart constructor.
data AccessControlList = AccessControlList'
  { -- | Specifies whether the ACL grants the general public with read access
    -- permissions for the bucket.
    allowsPublicReadAccess :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the ACL grants the general public with write access
    -- permissions for the bucket.
    allowsPublicWriteAccess :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessControlList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowsPublicReadAccess', 'accessControlList_allowsPublicReadAccess' - Specifies whether the ACL grants the general public with read access
-- permissions for the bucket.
--
-- 'allowsPublicWriteAccess', 'accessControlList_allowsPublicWriteAccess' - Specifies whether the ACL grants the general public with write access
-- permissions for the bucket.
newAccessControlList ::
  AccessControlList
newAccessControlList =
  AccessControlList'
    { allowsPublicReadAccess =
        Prelude.Nothing,
      allowsPublicWriteAccess = Prelude.Nothing
    }

-- | Specifies whether the ACL grants the general public with read access
-- permissions for the bucket.
accessControlList_allowsPublicReadAccess :: Lens.Lens' AccessControlList (Prelude.Maybe Prelude.Bool)
accessControlList_allowsPublicReadAccess = Lens.lens (\AccessControlList' {allowsPublicReadAccess} -> allowsPublicReadAccess) (\s@AccessControlList' {} a -> s {allowsPublicReadAccess = a} :: AccessControlList)

-- | Specifies whether the ACL grants the general public with write access
-- permissions for the bucket.
accessControlList_allowsPublicWriteAccess :: Lens.Lens' AccessControlList (Prelude.Maybe Prelude.Bool)
accessControlList_allowsPublicWriteAccess = Lens.lens (\AccessControlList' {allowsPublicWriteAccess} -> allowsPublicWriteAccess) (\s@AccessControlList' {} a -> s {allowsPublicWriteAccess = a} :: AccessControlList)

instance Data.FromJSON AccessControlList where
  parseJSON =
    Data.withObject
      "AccessControlList"
      ( \x ->
          AccessControlList'
            Prelude.<$> (x Data..:? "allowsPublicReadAccess")
            Prelude.<*> (x Data..:? "allowsPublicWriteAccess")
      )

instance Prelude.Hashable AccessControlList where
  hashWithSalt _salt AccessControlList' {..} =
    _salt
      `Prelude.hashWithSalt` allowsPublicReadAccess
      `Prelude.hashWithSalt` allowsPublicWriteAccess

instance Prelude.NFData AccessControlList where
  rnf AccessControlList' {..} =
    Prelude.rnf allowsPublicReadAccess
      `Prelude.seq` Prelude.rnf allowsPublicWriteAccess
