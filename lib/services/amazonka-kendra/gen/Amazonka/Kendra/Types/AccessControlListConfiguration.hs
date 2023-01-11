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
-- Module      : Amazonka.Kendra.Types.AccessControlListConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.AccessControlListConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Access Control List files for the documents in a data source. For the
-- format of the file, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/s3-acl.html Access control for S3 data sources>.
--
-- /See:/ 'newAccessControlListConfiguration' smart constructor.
data AccessControlListConfiguration = AccessControlListConfiguration'
  { -- | Path to the Amazon S3 bucket that contains the ACL files.
    keyPath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessControlListConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPath', 'accessControlListConfiguration_keyPath' - Path to the Amazon S3 bucket that contains the ACL files.
newAccessControlListConfiguration ::
  AccessControlListConfiguration
newAccessControlListConfiguration =
  AccessControlListConfiguration'
    { keyPath =
        Prelude.Nothing
    }

-- | Path to the Amazon S3 bucket that contains the ACL files.
accessControlListConfiguration_keyPath :: Lens.Lens' AccessControlListConfiguration (Prelude.Maybe Prelude.Text)
accessControlListConfiguration_keyPath = Lens.lens (\AccessControlListConfiguration' {keyPath} -> keyPath) (\s@AccessControlListConfiguration' {} a -> s {keyPath = a} :: AccessControlListConfiguration)

instance Data.FromJSON AccessControlListConfiguration where
  parseJSON =
    Data.withObject
      "AccessControlListConfiguration"
      ( \x ->
          AccessControlListConfiguration'
            Prelude.<$> (x Data..:? "KeyPath")
      )

instance
  Prelude.Hashable
    AccessControlListConfiguration
  where
  hashWithSalt
    _salt
    AccessControlListConfiguration' {..} =
      _salt `Prelude.hashWithSalt` keyPath

instance
  Prelude.NFData
    AccessControlListConfiguration
  where
  rnf AccessControlListConfiguration' {..} =
    Prelude.rnf keyPath

instance Data.ToJSON AccessControlListConfiguration where
  toJSON AccessControlListConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("KeyPath" Data..=) Prelude.<$> keyPath]
      )
