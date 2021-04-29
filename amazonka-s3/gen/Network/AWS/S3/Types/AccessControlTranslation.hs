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
-- Module      : Network.AWS.S3.Types.AccessControlTranslation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AccessControlTranslation where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.OwnerOverride

-- | A container for information about access control for replicas.
--
-- /See:/ 'newAccessControlTranslation' smart constructor.
data AccessControlTranslation = AccessControlTranslation'
  { -- | Specifies the replica ownership. For default and valid values, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT bucket replication>
    -- in the /Amazon Simple Storage Service API Reference/.
    owner :: OwnerOverride
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AccessControlTranslation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'owner', 'accessControlTranslation_owner' - Specifies the replica ownership. For default and valid values, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT bucket replication>
-- in the /Amazon Simple Storage Service API Reference/.
newAccessControlTranslation ::
  -- | 'owner'
  OwnerOverride ->
  AccessControlTranslation
newAccessControlTranslation pOwner_ =
  AccessControlTranslation' {owner = pOwner_}

-- | Specifies the replica ownership. For default and valid values, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT bucket replication>
-- in the /Amazon Simple Storage Service API Reference/.
accessControlTranslation_owner :: Lens.Lens' AccessControlTranslation OwnerOverride
accessControlTranslation_owner = Lens.lens (\AccessControlTranslation' {owner} -> owner) (\s@AccessControlTranslation' {} a -> s {owner = a} :: AccessControlTranslation)

instance Prelude.FromXML AccessControlTranslation where
  parseXML x =
    AccessControlTranslation'
      Prelude.<$> (x Prelude..@ "Owner")

instance Prelude.Hashable AccessControlTranslation

instance Prelude.NFData AccessControlTranslation

instance Prelude.ToXML AccessControlTranslation where
  toXML AccessControlTranslation' {..} =
    Prelude.mconcat ["Owner" Prelude.@= owner]
