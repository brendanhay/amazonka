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
-- Module      : Amazonka.GuardDuty.Types.AccessControlList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.AccessControlList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information on the current access control policies for the
-- bucket.
--
-- /See:/ 'newAccessControlList' smart constructor.
data AccessControlList = AccessControlList'
  { -- | A value that indicates whether public read access for the bucket is
    -- enabled through an Access Control List (ACL).
    allowsPublicReadAccess :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether public write access for the bucket is
    -- enabled through an Access Control List (ACL).
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
-- 'allowsPublicReadAccess', 'accessControlList_allowsPublicReadAccess' - A value that indicates whether public read access for the bucket is
-- enabled through an Access Control List (ACL).
--
-- 'allowsPublicWriteAccess', 'accessControlList_allowsPublicWriteAccess' - A value that indicates whether public write access for the bucket is
-- enabled through an Access Control List (ACL).
newAccessControlList ::
  AccessControlList
newAccessControlList =
  AccessControlList'
    { allowsPublicReadAccess =
        Prelude.Nothing,
      allowsPublicWriteAccess = Prelude.Nothing
    }

-- | A value that indicates whether public read access for the bucket is
-- enabled through an Access Control List (ACL).
accessControlList_allowsPublicReadAccess :: Lens.Lens' AccessControlList (Prelude.Maybe Prelude.Bool)
accessControlList_allowsPublicReadAccess = Lens.lens (\AccessControlList' {allowsPublicReadAccess} -> allowsPublicReadAccess) (\s@AccessControlList' {} a -> s {allowsPublicReadAccess = a} :: AccessControlList)

-- | A value that indicates whether public write access for the bucket is
-- enabled through an Access Control List (ACL).
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
    Prelude.rnf allowsPublicReadAccess `Prelude.seq`
      Prelude.rnf allowsPublicWriteAccess
