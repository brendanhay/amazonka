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
-- Module      : Amazonka.SecurityLake.Types.CustomLogSourceProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.CustomLogSourceProvider where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of the log provider for a third-party custom source.
--
-- /See:/ 'newCustomLogSourceProvider' smart constructor.
data CustomLogSourceProvider = CustomLogSourceProvider'
  { -- | The location of the partition in the Amazon S3 bucket for Security Lake.
    location :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role to be used by the entity putting logs into your
    -- custom source partition. Security Lake will apply the correct access
    -- policies to this role, but you must first manually create the trust
    -- policy for this role. The IAM role name must start with the text
    -- \'Security Lake\'. The IAM role must trust the @logProviderAccountId@ to
    -- assume the role.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomLogSourceProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'customLogSourceProvider_location' - The location of the partition in the Amazon S3 bucket for Security Lake.
--
-- 'roleArn', 'customLogSourceProvider_roleArn' - The ARN of the IAM role to be used by the entity putting logs into your
-- custom source partition. Security Lake will apply the correct access
-- policies to this role, but you must first manually create the trust
-- policy for this role. The IAM role name must start with the text
-- \'Security Lake\'. The IAM role must trust the @logProviderAccountId@ to
-- assume the role.
newCustomLogSourceProvider ::
  CustomLogSourceProvider
newCustomLogSourceProvider =
  CustomLogSourceProvider'
    { location =
        Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The location of the partition in the Amazon S3 bucket for Security Lake.
customLogSourceProvider_location :: Lens.Lens' CustomLogSourceProvider (Prelude.Maybe Prelude.Text)
customLogSourceProvider_location = Lens.lens (\CustomLogSourceProvider' {location} -> location) (\s@CustomLogSourceProvider' {} a -> s {location = a} :: CustomLogSourceProvider)

-- | The ARN of the IAM role to be used by the entity putting logs into your
-- custom source partition. Security Lake will apply the correct access
-- policies to this role, but you must first manually create the trust
-- policy for this role. The IAM role name must start with the text
-- \'Security Lake\'. The IAM role must trust the @logProviderAccountId@ to
-- assume the role.
customLogSourceProvider_roleArn :: Lens.Lens' CustomLogSourceProvider (Prelude.Maybe Prelude.Text)
customLogSourceProvider_roleArn = Lens.lens (\CustomLogSourceProvider' {roleArn} -> roleArn) (\s@CustomLogSourceProvider' {} a -> s {roleArn = a} :: CustomLogSourceProvider)

instance Data.FromJSON CustomLogSourceProvider where
  parseJSON =
    Data.withObject
      "CustomLogSourceProvider"
      ( \x ->
          CustomLogSourceProvider'
            Prelude.<$> (x Data..:? "location")
            Prelude.<*> (x Data..:? "roleArn")
      )

instance Prelude.Hashable CustomLogSourceProvider where
  hashWithSalt _salt CustomLogSourceProvider' {..} =
    _salt
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CustomLogSourceProvider where
  rnf CustomLogSourceProvider' {..} =
    Prelude.rnf location
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON CustomLogSourceProvider where
  toJSON CustomLogSourceProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("location" Data..=) Prelude.<$> location,
            ("roleArn" Data..=) Prelude.<$> roleArn
          ]
      )
