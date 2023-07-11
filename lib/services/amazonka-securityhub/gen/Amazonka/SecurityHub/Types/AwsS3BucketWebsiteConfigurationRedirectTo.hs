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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRedirectTo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRedirectTo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The redirect behavior for requests to the website.
--
-- /See:/ 'newAwsS3BucketWebsiteConfigurationRedirectTo' smart constructor.
data AwsS3BucketWebsiteConfigurationRedirectTo = AwsS3BucketWebsiteConfigurationRedirectTo'
  { -- | The name of the host to redirect requests to.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | The protocol to use when redirecting requests. By default, this field
    -- uses the same protocol as the original request. Valid values are @http@
    -- or @https@.
    protocol :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketWebsiteConfigurationRedirectTo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostname', 'awsS3BucketWebsiteConfigurationRedirectTo_hostname' - The name of the host to redirect requests to.
--
-- 'protocol', 'awsS3BucketWebsiteConfigurationRedirectTo_protocol' - The protocol to use when redirecting requests. By default, this field
-- uses the same protocol as the original request. Valid values are @http@
-- or @https@.
newAwsS3BucketWebsiteConfigurationRedirectTo ::
  AwsS3BucketWebsiteConfigurationRedirectTo
newAwsS3BucketWebsiteConfigurationRedirectTo =
  AwsS3BucketWebsiteConfigurationRedirectTo'
    { hostname =
        Prelude.Nothing,
      protocol = Prelude.Nothing
    }

-- | The name of the host to redirect requests to.
awsS3BucketWebsiteConfigurationRedirectTo_hostname :: Lens.Lens' AwsS3BucketWebsiteConfigurationRedirectTo (Prelude.Maybe Prelude.Text)
awsS3BucketWebsiteConfigurationRedirectTo_hostname = Lens.lens (\AwsS3BucketWebsiteConfigurationRedirectTo' {hostname} -> hostname) (\s@AwsS3BucketWebsiteConfigurationRedirectTo' {} a -> s {hostname = a} :: AwsS3BucketWebsiteConfigurationRedirectTo)

-- | The protocol to use when redirecting requests. By default, this field
-- uses the same protocol as the original request. Valid values are @http@
-- or @https@.
awsS3BucketWebsiteConfigurationRedirectTo_protocol :: Lens.Lens' AwsS3BucketWebsiteConfigurationRedirectTo (Prelude.Maybe Prelude.Text)
awsS3BucketWebsiteConfigurationRedirectTo_protocol = Lens.lens (\AwsS3BucketWebsiteConfigurationRedirectTo' {protocol} -> protocol) (\s@AwsS3BucketWebsiteConfigurationRedirectTo' {} a -> s {protocol = a} :: AwsS3BucketWebsiteConfigurationRedirectTo)

instance
  Data.FromJSON
    AwsS3BucketWebsiteConfigurationRedirectTo
  where
  parseJSON =
    Data.withObject
      "AwsS3BucketWebsiteConfigurationRedirectTo"
      ( \x ->
          AwsS3BucketWebsiteConfigurationRedirectTo'
            Prelude.<$> (x Data..:? "Hostname")
            Prelude.<*> (x Data..:? "Protocol")
      )

instance
  Prelude.Hashable
    AwsS3BucketWebsiteConfigurationRedirectTo
  where
  hashWithSalt
    _salt
    AwsS3BucketWebsiteConfigurationRedirectTo' {..} =
      _salt
        `Prelude.hashWithSalt` hostname
        `Prelude.hashWithSalt` protocol

instance
  Prelude.NFData
    AwsS3BucketWebsiteConfigurationRedirectTo
  where
  rnf AwsS3BucketWebsiteConfigurationRedirectTo' {..} =
    Prelude.rnf hostname
      `Prelude.seq` Prelude.rnf protocol

instance
  Data.ToJSON
    AwsS3BucketWebsiteConfigurationRedirectTo
  where
  toJSON AwsS3BucketWebsiteConfigurationRedirectTo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Hostname" Data..=) Prelude.<$> hostname,
            ("Protocol" Data..=) Prelude.<$> protocol
          ]
      )
