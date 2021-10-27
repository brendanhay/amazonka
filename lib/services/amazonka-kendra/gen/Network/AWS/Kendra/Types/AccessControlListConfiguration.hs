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
-- Module      : Network.AWS.Kendra.Types.AccessControlListConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.AccessControlListConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Access Control List files for the documents in a data source. For the
-- format of the file, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/s3-acl.html Access control for S3 data sources>.
--
-- /See:/ 'newAccessControlListConfiguration' smart constructor.
data AccessControlListConfiguration = AccessControlListConfiguration'
  { -- | Path to the Amazon Web Services S3 bucket that contains the ACL files.
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
-- 'keyPath', 'accessControlListConfiguration_keyPath' - Path to the Amazon Web Services S3 bucket that contains the ACL files.
newAccessControlListConfiguration ::
  AccessControlListConfiguration
newAccessControlListConfiguration =
  AccessControlListConfiguration'
    { keyPath =
        Prelude.Nothing
    }

-- | Path to the Amazon Web Services S3 bucket that contains the ACL files.
accessControlListConfiguration_keyPath :: Lens.Lens' AccessControlListConfiguration (Prelude.Maybe Prelude.Text)
accessControlListConfiguration_keyPath = Lens.lens (\AccessControlListConfiguration' {keyPath} -> keyPath) (\s@AccessControlListConfiguration' {} a -> s {keyPath = a} :: AccessControlListConfiguration)

instance Core.FromJSON AccessControlListConfiguration where
  parseJSON =
    Core.withObject
      "AccessControlListConfiguration"
      ( \x ->
          AccessControlListConfiguration'
            Prelude.<$> (x Core..:? "KeyPath")
      )

instance
  Prelude.Hashable
    AccessControlListConfiguration

instance
  Prelude.NFData
    AccessControlListConfiguration

instance Core.ToJSON AccessControlListConfiguration where
  toJSON AccessControlListConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [("KeyPath" Core..=) Prelude.<$> keyPath]
      )
