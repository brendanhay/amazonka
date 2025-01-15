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
-- Module      : Amazonka.EC2.Types.VerifiedAccessLogS3DestinationOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessLogS3DestinationOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Options for Amazon S3 as a logging destination.
--
-- /See:/ 'newVerifiedAccessLogS3DestinationOptions' smart constructor.
data VerifiedAccessLogS3DestinationOptions = VerifiedAccessLogS3DestinationOptions'
  { -- | The bucket name.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the Amazon S3
    -- bucket.
    bucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The bucket prefix.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether logging is enabled.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifiedAccessLogS3DestinationOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 'verifiedAccessLogS3DestinationOptions_bucketName' - The bucket name.
--
-- 'bucketOwner', 'verifiedAccessLogS3DestinationOptions_bucketOwner' - The ID of the Amazon Web Services account that owns the Amazon S3
-- bucket.
--
-- 'prefix', 'verifiedAccessLogS3DestinationOptions_prefix' - The bucket prefix.
--
-- 'enabled', 'verifiedAccessLogS3DestinationOptions_enabled' - Indicates whether logging is enabled.
newVerifiedAccessLogS3DestinationOptions ::
  -- | 'enabled'
  Prelude.Bool ->
  VerifiedAccessLogS3DestinationOptions
newVerifiedAccessLogS3DestinationOptions pEnabled_ =
  VerifiedAccessLogS3DestinationOptions'
    { bucketName =
        Prelude.Nothing,
      bucketOwner = Prelude.Nothing,
      prefix = Prelude.Nothing,
      enabled = pEnabled_
    }

-- | The bucket name.
verifiedAccessLogS3DestinationOptions_bucketName :: Lens.Lens' VerifiedAccessLogS3DestinationOptions (Prelude.Maybe Prelude.Text)
verifiedAccessLogS3DestinationOptions_bucketName = Lens.lens (\VerifiedAccessLogS3DestinationOptions' {bucketName} -> bucketName) (\s@VerifiedAccessLogS3DestinationOptions' {} a -> s {bucketName = a} :: VerifiedAccessLogS3DestinationOptions)

-- | The ID of the Amazon Web Services account that owns the Amazon S3
-- bucket.
verifiedAccessLogS3DestinationOptions_bucketOwner :: Lens.Lens' VerifiedAccessLogS3DestinationOptions (Prelude.Maybe Prelude.Text)
verifiedAccessLogS3DestinationOptions_bucketOwner = Lens.lens (\VerifiedAccessLogS3DestinationOptions' {bucketOwner} -> bucketOwner) (\s@VerifiedAccessLogS3DestinationOptions' {} a -> s {bucketOwner = a} :: VerifiedAccessLogS3DestinationOptions)

-- | The bucket prefix.
verifiedAccessLogS3DestinationOptions_prefix :: Lens.Lens' VerifiedAccessLogS3DestinationOptions (Prelude.Maybe Prelude.Text)
verifiedAccessLogS3DestinationOptions_prefix = Lens.lens (\VerifiedAccessLogS3DestinationOptions' {prefix} -> prefix) (\s@VerifiedAccessLogS3DestinationOptions' {} a -> s {prefix = a} :: VerifiedAccessLogS3DestinationOptions)

-- | Indicates whether logging is enabled.
verifiedAccessLogS3DestinationOptions_enabled :: Lens.Lens' VerifiedAccessLogS3DestinationOptions Prelude.Bool
verifiedAccessLogS3DestinationOptions_enabled = Lens.lens (\VerifiedAccessLogS3DestinationOptions' {enabled} -> enabled) (\s@VerifiedAccessLogS3DestinationOptions' {} a -> s {enabled = a} :: VerifiedAccessLogS3DestinationOptions)

instance
  Prelude.Hashable
    VerifiedAccessLogS3DestinationOptions
  where
  hashWithSalt
    _salt
    VerifiedAccessLogS3DestinationOptions' {..} =
      _salt
        `Prelude.hashWithSalt` bucketName
        `Prelude.hashWithSalt` bucketOwner
        `Prelude.hashWithSalt` prefix
        `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    VerifiedAccessLogS3DestinationOptions
  where
  rnf VerifiedAccessLogS3DestinationOptions' {..} =
    Prelude.rnf bucketName `Prelude.seq`
      Prelude.rnf bucketOwner `Prelude.seq`
        Prelude.rnf prefix `Prelude.seq`
          Prelude.rnf enabled

instance
  Data.ToQuery
    VerifiedAccessLogS3DestinationOptions
  where
  toQuery VerifiedAccessLogS3DestinationOptions' {..} =
    Prelude.mconcat
      [ "BucketName" Data.=: bucketName,
        "BucketOwner" Data.=: bucketOwner,
        "Prefix" Data.=: prefix,
        "Enabled" Data.=: enabled
      ]
