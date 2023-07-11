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
-- Module      : Amazonka.Athena.Types.AclConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.AclConfiguration where

import Amazonka.Athena.Types.S3AclOption
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates that an Amazon S3 canned ACL should be set to control
-- ownership of stored query results. When Athena stores query results in
-- Amazon S3, the canned ACL is set with the @x-amz-acl@ request header.
-- For more information about S3 Object Ownership, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/about-object-ownership.html#object-ownership-overview Object Ownership settings>
-- in the /Amazon S3 User Guide/.
--
-- /See:/ 'newAclConfiguration' smart constructor.
data AclConfiguration = AclConfiguration'
  { -- | The Amazon S3 canned ACL that Athena should specify when storing query
    -- results. Currently the only supported canned ACL is
    -- @BUCKET_OWNER_FULL_CONTROL@. If a query runs in a workgroup and the
    -- workgroup overrides client-side settings, then the Amazon S3 canned ACL
    -- specified in the workgroup\'s settings is used for all queries that run
    -- in the workgroup. For more information about Amazon S3 canned ACLs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/acl-overview.html#canned-acl Canned ACL>
    -- in the /Amazon S3 User Guide/.
    s3AclOption :: S3AclOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AclConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3AclOption', 'aclConfiguration_s3AclOption' - The Amazon S3 canned ACL that Athena should specify when storing query
-- results. Currently the only supported canned ACL is
-- @BUCKET_OWNER_FULL_CONTROL@. If a query runs in a workgroup and the
-- workgroup overrides client-side settings, then the Amazon S3 canned ACL
-- specified in the workgroup\'s settings is used for all queries that run
-- in the workgroup. For more information about Amazon S3 canned ACLs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/acl-overview.html#canned-acl Canned ACL>
-- in the /Amazon S3 User Guide/.
newAclConfiguration ::
  -- | 's3AclOption'
  S3AclOption ->
  AclConfiguration
newAclConfiguration pS3AclOption_ =
  AclConfiguration' {s3AclOption = pS3AclOption_}

-- | The Amazon S3 canned ACL that Athena should specify when storing query
-- results. Currently the only supported canned ACL is
-- @BUCKET_OWNER_FULL_CONTROL@. If a query runs in a workgroup and the
-- workgroup overrides client-side settings, then the Amazon S3 canned ACL
-- specified in the workgroup\'s settings is used for all queries that run
-- in the workgroup. For more information about Amazon S3 canned ACLs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/acl-overview.html#canned-acl Canned ACL>
-- in the /Amazon S3 User Guide/.
aclConfiguration_s3AclOption :: Lens.Lens' AclConfiguration S3AclOption
aclConfiguration_s3AclOption = Lens.lens (\AclConfiguration' {s3AclOption} -> s3AclOption) (\s@AclConfiguration' {} a -> s {s3AclOption = a} :: AclConfiguration)

instance Data.FromJSON AclConfiguration where
  parseJSON =
    Data.withObject
      "AclConfiguration"
      ( \x ->
          AclConfiguration'
            Prelude.<$> (x Data..: "S3AclOption")
      )

instance Prelude.Hashable AclConfiguration where
  hashWithSalt _salt AclConfiguration' {..} =
    _salt `Prelude.hashWithSalt` s3AclOption

instance Prelude.NFData AclConfiguration where
  rnf AclConfiguration' {..} = Prelude.rnf s3AclOption

instance Data.ToJSON AclConfiguration where
  toJSON AclConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3AclOption" Data..= s3AclOption)]
      )
