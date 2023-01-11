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
-- Module      : Amazonka.SecurityHub.Types.AwsS3AccountPublicAccessBlockDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3AccountPublicAccessBlockDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | provides information about the Amazon S3 Public Access Block
-- configuration for accounts.
--
-- /See:/ 'newAwsS3AccountPublicAccessBlockDetails' smart constructor.
data AwsS3AccountPublicAccessBlockDetails = AwsS3AccountPublicAccessBlockDetails'
  { -- | Indicates whether to reject calls to update an S3 bucket if the calls
    -- include a public access control list (ACL).
    blockPublicAcls :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to reject calls to update the access policy for an S3
    -- bucket or access point if the policy allows public access.
    blockPublicPolicy :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether Amazon S3 ignores public ACLs that are associated with
    -- an S3 bucket.
    ignorePublicAcls :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to restrict access to an access point or S3 bucket
    -- that has a public policy to only Amazon Web Services service principals
    -- and authorized users within the S3 bucket owner\'s account.
    restrictPublicBuckets :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3AccountPublicAccessBlockDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockPublicAcls', 'awsS3AccountPublicAccessBlockDetails_blockPublicAcls' - Indicates whether to reject calls to update an S3 bucket if the calls
-- include a public access control list (ACL).
--
-- 'blockPublicPolicy', 'awsS3AccountPublicAccessBlockDetails_blockPublicPolicy' - Indicates whether to reject calls to update the access policy for an S3
-- bucket or access point if the policy allows public access.
--
-- 'ignorePublicAcls', 'awsS3AccountPublicAccessBlockDetails_ignorePublicAcls' - Indicates whether Amazon S3 ignores public ACLs that are associated with
-- an S3 bucket.
--
-- 'restrictPublicBuckets', 'awsS3AccountPublicAccessBlockDetails_restrictPublicBuckets' - Indicates whether to restrict access to an access point or S3 bucket
-- that has a public policy to only Amazon Web Services service principals
-- and authorized users within the S3 bucket owner\'s account.
newAwsS3AccountPublicAccessBlockDetails ::
  AwsS3AccountPublicAccessBlockDetails
newAwsS3AccountPublicAccessBlockDetails =
  AwsS3AccountPublicAccessBlockDetails'
    { blockPublicAcls =
        Prelude.Nothing,
      blockPublicPolicy = Prelude.Nothing,
      ignorePublicAcls = Prelude.Nothing,
      restrictPublicBuckets =
        Prelude.Nothing
    }

-- | Indicates whether to reject calls to update an S3 bucket if the calls
-- include a public access control list (ACL).
awsS3AccountPublicAccessBlockDetails_blockPublicAcls :: Lens.Lens' AwsS3AccountPublicAccessBlockDetails (Prelude.Maybe Prelude.Bool)
awsS3AccountPublicAccessBlockDetails_blockPublicAcls = Lens.lens (\AwsS3AccountPublicAccessBlockDetails' {blockPublicAcls} -> blockPublicAcls) (\s@AwsS3AccountPublicAccessBlockDetails' {} a -> s {blockPublicAcls = a} :: AwsS3AccountPublicAccessBlockDetails)

-- | Indicates whether to reject calls to update the access policy for an S3
-- bucket or access point if the policy allows public access.
awsS3AccountPublicAccessBlockDetails_blockPublicPolicy :: Lens.Lens' AwsS3AccountPublicAccessBlockDetails (Prelude.Maybe Prelude.Bool)
awsS3AccountPublicAccessBlockDetails_blockPublicPolicy = Lens.lens (\AwsS3AccountPublicAccessBlockDetails' {blockPublicPolicy} -> blockPublicPolicy) (\s@AwsS3AccountPublicAccessBlockDetails' {} a -> s {blockPublicPolicy = a} :: AwsS3AccountPublicAccessBlockDetails)

-- | Indicates whether Amazon S3 ignores public ACLs that are associated with
-- an S3 bucket.
awsS3AccountPublicAccessBlockDetails_ignorePublicAcls :: Lens.Lens' AwsS3AccountPublicAccessBlockDetails (Prelude.Maybe Prelude.Bool)
awsS3AccountPublicAccessBlockDetails_ignorePublicAcls = Lens.lens (\AwsS3AccountPublicAccessBlockDetails' {ignorePublicAcls} -> ignorePublicAcls) (\s@AwsS3AccountPublicAccessBlockDetails' {} a -> s {ignorePublicAcls = a} :: AwsS3AccountPublicAccessBlockDetails)

-- | Indicates whether to restrict access to an access point or S3 bucket
-- that has a public policy to only Amazon Web Services service principals
-- and authorized users within the S3 bucket owner\'s account.
awsS3AccountPublicAccessBlockDetails_restrictPublicBuckets :: Lens.Lens' AwsS3AccountPublicAccessBlockDetails (Prelude.Maybe Prelude.Bool)
awsS3AccountPublicAccessBlockDetails_restrictPublicBuckets = Lens.lens (\AwsS3AccountPublicAccessBlockDetails' {restrictPublicBuckets} -> restrictPublicBuckets) (\s@AwsS3AccountPublicAccessBlockDetails' {} a -> s {restrictPublicBuckets = a} :: AwsS3AccountPublicAccessBlockDetails)

instance
  Data.FromJSON
    AwsS3AccountPublicAccessBlockDetails
  where
  parseJSON =
    Data.withObject
      "AwsS3AccountPublicAccessBlockDetails"
      ( \x ->
          AwsS3AccountPublicAccessBlockDetails'
            Prelude.<$> (x Data..:? "BlockPublicAcls")
            Prelude.<*> (x Data..:? "BlockPublicPolicy")
            Prelude.<*> (x Data..:? "IgnorePublicAcls")
            Prelude.<*> (x Data..:? "RestrictPublicBuckets")
      )

instance
  Prelude.Hashable
    AwsS3AccountPublicAccessBlockDetails
  where
  hashWithSalt
    _salt
    AwsS3AccountPublicAccessBlockDetails' {..} =
      _salt `Prelude.hashWithSalt` blockPublicAcls
        `Prelude.hashWithSalt` blockPublicPolicy
        `Prelude.hashWithSalt` ignorePublicAcls
        `Prelude.hashWithSalt` restrictPublicBuckets

instance
  Prelude.NFData
    AwsS3AccountPublicAccessBlockDetails
  where
  rnf AwsS3AccountPublicAccessBlockDetails' {..} =
    Prelude.rnf blockPublicAcls
      `Prelude.seq` Prelude.rnf blockPublicPolicy
      `Prelude.seq` Prelude.rnf ignorePublicAcls
      `Prelude.seq` Prelude.rnf restrictPublicBuckets

instance
  Data.ToJSON
    AwsS3AccountPublicAccessBlockDetails
  where
  toJSON AwsS3AccountPublicAccessBlockDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BlockPublicAcls" Data..=)
              Prelude.<$> blockPublicAcls,
            ("BlockPublicPolicy" Data..=)
              Prelude.<$> blockPublicPolicy,
            ("IgnorePublicAcls" Data..=)
              Prelude.<$> ignorePublicAcls,
            ("RestrictPublicBuckets" Data..=)
              Prelude.<$> restrictPublicBuckets
          ]
      )
