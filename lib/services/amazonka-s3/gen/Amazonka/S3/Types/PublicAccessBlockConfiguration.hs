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
-- Module      : Amazonka.S3.Types.PublicAccessBlockConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.PublicAccessBlockConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | The PublicAccessBlock configuration that you want to apply to this
-- Amazon S3 bucket. You can enable the configuration options in any
-- combination. For more information about when Amazon S3 considers a
-- bucket or object public, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of \"Public\">
-- in the /Amazon S3 User Guide/.
--
-- /See:/ 'newPublicAccessBlockConfiguration' smart constructor.
data PublicAccessBlockConfiguration = PublicAccessBlockConfiguration'
  { -- | Specifies whether Amazon S3 should block public access control lists
    -- (ACLs) for this bucket and objects in this bucket. Setting this element
    -- to @TRUE@ causes the following behavior:
    --
    -- -   PUT Bucket ACL and PUT Object ACL calls fail if the specified ACL is
    --     public.
    --
    -- -   PUT Object calls fail if the request includes a public ACL.
    --
    -- -   PUT Bucket calls fail if the request includes a public ACL.
    --
    -- Enabling this setting doesn\'t affect existing policies or ACLs.
    blockPublicAcls :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether Amazon S3 should block public bucket policies for this
    -- bucket. Setting this element to @TRUE@ causes Amazon S3 to reject calls
    -- to PUT Bucket policy if the specified bucket policy allows public
    -- access.
    --
    -- Enabling this setting doesn\'t affect existing bucket policies.
    blockPublicPolicy :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether Amazon S3 should ignore public ACLs for this bucket
    -- and objects in this bucket. Setting this element to @TRUE@ causes Amazon
    -- S3 to ignore all public ACLs on this bucket and objects in this bucket.
    --
    -- Enabling this setting doesn\'t affect the persistence of any existing
    -- ACLs and doesn\'t prevent new public ACLs from being set.
    ignorePublicAcls :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether Amazon S3 should restrict public bucket policies for
    -- this bucket. Setting this element to @TRUE@ restricts access to this
    -- bucket to only Amazon Web Service principals and authorized users within
    -- this account if the bucket has a public policy.
    --
    -- Enabling this setting doesn\'t affect previously stored bucket policies,
    -- except that public and cross-account access within any public bucket
    -- policy, including non-public delegation to specific accounts, is
    -- blocked.
    restrictPublicBuckets :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublicAccessBlockConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockPublicAcls', 'publicAccessBlockConfiguration_blockPublicAcls' - Specifies whether Amazon S3 should block public access control lists
-- (ACLs) for this bucket and objects in this bucket. Setting this element
-- to @TRUE@ causes the following behavior:
--
-- -   PUT Bucket ACL and PUT Object ACL calls fail if the specified ACL is
--     public.
--
-- -   PUT Object calls fail if the request includes a public ACL.
--
-- -   PUT Bucket calls fail if the request includes a public ACL.
--
-- Enabling this setting doesn\'t affect existing policies or ACLs.
--
-- 'blockPublicPolicy', 'publicAccessBlockConfiguration_blockPublicPolicy' - Specifies whether Amazon S3 should block public bucket policies for this
-- bucket. Setting this element to @TRUE@ causes Amazon S3 to reject calls
-- to PUT Bucket policy if the specified bucket policy allows public
-- access.
--
-- Enabling this setting doesn\'t affect existing bucket policies.
--
-- 'ignorePublicAcls', 'publicAccessBlockConfiguration_ignorePublicAcls' - Specifies whether Amazon S3 should ignore public ACLs for this bucket
-- and objects in this bucket. Setting this element to @TRUE@ causes Amazon
-- S3 to ignore all public ACLs on this bucket and objects in this bucket.
--
-- Enabling this setting doesn\'t affect the persistence of any existing
-- ACLs and doesn\'t prevent new public ACLs from being set.
--
-- 'restrictPublicBuckets', 'publicAccessBlockConfiguration_restrictPublicBuckets' - Specifies whether Amazon S3 should restrict public bucket policies for
-- this bucket. Setting this element to @TRUE@ restricts access to this
-- bucket to only Amazon Web Service principals and authorized users within
-- this account if the bucket has a public policy.
--
-- Enabling this setting doesn\'t affect previously stored bucket policies,
-- except that public and cross-account access within any public bucket
-- policy, including non-public delegation to specific accounts, is
-- blocked.
newPublicAccessBlockConfiguration ::
  PublicAccessBlockConfiguration
newPublicAccessBlockConfiguration =
  PublicAccessBlockConfiguration'
    { blockPublicAcls =
        Prelude.Nothing,
      blockPublicPolicy = Prelude.Nothing,
      ignorePublicAcls = Prelude.Nothing,
      restrictPublicBuckets = Prelude.Nothing
    }

-- | Specifies whether Amazon S3 should block public access control lists
-- (ACLs) for this bucket and objects in this bucket. Setting this element
-- to @TRUE@ causes the following behavior:
--
-- -   PUT Bucket ACL and PUT Object ACL calls fail if the specified ACL is
--     public.
--
-- -   PUT Object calls fail if the request includes a public ACL.
--
-- -   PUT Bucket calls fail if the request includes a public ACL.
--
-- Enabling this setting doesn\'t affect existing policies or ACLs.
publicAccessBlockConfiguration_blockPublicAcls :: Lens.Lens' PublicAccessBlockConfiguration (Prelude.Maybe Prelude.Bool)
publicAccessBlockConfiguration_blockPublicAcls = Lens.lens (\PublicAccessBlockConfiguration' {blockPublicAcls} -> blockPublicAcls) (\s@PublicAccessBlockConfiguration' {} a -> s {blockPublicAcls = a} :: PublicAccessBlockConfiguration)

-- | Specifies whether Amazon S3 should block public bucket policies for this
-- bucket. Setting this element to @TRUE@ causes Amazon S3 to reject calls
-- to PUT Bucket policy if the specified bucket policy allows public
-- access.
--
-- Enabling this setting doesn\'t affect existing bucket policies.
publicAccessBlockConfiguration_blockPublicPolicy :: Lens.Lens' PublicAccessBlockConfiguration (Prelude.Maybe Prelude.Bool)
publicAccessBlockConfiguration_blockPublicPolicy = Lens.lens (\PublicAccessBlockConfiguration' {blockPublicPolicy} -> blockPublicPolicy) (\s@PublicAccessBlockConfiguration' {} a -> s {blockPublicPolicy = a} :: PublicAccessBlockConfiguration)

-- | Specifies whether Amazon S3 should ignore public ACLs for this bucket
-- and objects in this bucket. Setting this element to @TRUE@ causes Amazon
-- S3 to ignore all public ACLs on this bucket and objects in this bucket.
--
-- Enabling this setting doesn\'t affect the persistence of any existing
-- ACLs and doesn\'t prevent new public ACLs from being set.
publicAccessBlockConfiguration_ignorePublicAcls :: Lens.Lens' PublicAccessBlockConfiguration (Prelude.Maybe Prelude.Bool)
publicAccessBlockConfiguration_ignorePublicAcls = Lens.lens (\PublicAccessBlockConfiguration' {ignorePublicAcls} -> ignorePublicAcls) (\s@PublicAccessBlockConfiguration' {} a -> s {ignorePublicAcls = a} :: PublicAccessBlockConfiguration)

-- | Specifies whether Amazon S3 should restrict public bucket policies for
-- this bucket. Setting this element to @TRUE@ restricts access to this
-- bucket to only Amazon Web Service principals and authorized users within
-- this account if the bucket has a public policy.
--
-- Enabling this setting doesn\'t affect previously stored bucket policies,
-- except that public and cross-account access within any public bucket
-- policy, including non-public delegation to specific accounts, is
-- blocked.
publicAccessBlockConfiguration_restrictPublicBuckets :: Lens.Lens' PublicAccessBlockConfiguration (Prelude.Maybe Prelude.Bool)
publicAccessBlockConfiguration_restrictPublicBuckets = Lens.lens (\PublicAccessBlockConfiguration' {restrictPublicBuckets} -> restrictPublicBuckets) (\s@PublicAccessBlockConfiguration' {} a -> s {restrictPublicBuckets = a} :: PublicAccessBlockConfiguration)

instance Data.FromXML PublicAccessBlockConfiguration where
  parseXML x =
    PublicAccessBlockConfiguration'
      Prelude.<$> (x Data..@? "BlockPublicAcls")
      Prelude.<*> (x Data..@? "BlockPublicPolicy")
      Prelude.<*> (x Data..@? "IgnorePublicAcls")
      Prelude.<*> (x Data..@? "RestrictPublicBuckets")

instance
  Prelude.Hashable
    PublicAccessBlockConfiguration
  where
  hashWithSalt
    _salt
    PublicAccessBlockConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` blockPublicAcls
        `Prelude.hashWithSalt` blockPublicPolicy
        `Prelude.hashWithSalt` ignorePublicAcls
        `Prelude.hashWithSalt` restrictPublicBuckets

instance
  Prelude.NFData
    PublicAccessBlockConfiguration
  where
  rnf PublicAccessBlockConfiguration' {..} =
    Prelude.rnf blockPublicAcls
      `Prelude.seq` Prelude.rnf blockPublicPolicy
      `Prelude.seq` Prelude.rnf ignorePublicAcls
      `Prelude.seq` Prelude.rnf restrictPublicBuckets

instance Data.ToXML PublicAccessBlockConfiguration where
  toXML PublicAccessBlockConfiguration' {..} =
    Prelude.mconcat
      [ "BlockPublicAcls" Data.@= blockPublicAcls,
        "BlockPublicPolicy" Data.@= blockPublicPolicy,
        "IgnorePublicAcls" Data.@= ignorePublicAcls,
        "RestrictPublicBuckets"
          Data.@= restrictPublicBuckets
      ]
