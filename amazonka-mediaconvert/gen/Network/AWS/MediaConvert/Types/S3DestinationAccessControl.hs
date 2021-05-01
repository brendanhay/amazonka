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
-- Module      : Network.AWS.MediaConvert.Types.S3DestinationAccessControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.S3DestinationAccessControl where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.S3ObjectCannedAcl
import qualified Network.AWS.Prelude as Prelude

-- | Optional. Have MediaConvert automatically apply Amazon S3 access control
-- for the outputs in this output group. When you don\'t use this setting,
-- S3 automatically applies the default access control list PRIVATE.
--
-- /See:/ 'newS3DestinationAccessControl' smart constructor.
data S3DestinationAccessControl = S3DestinationAccessControl'
  { -- | Choose an Amazon S3 canned ACL for MediaConvert to apply to this output.
    cannedAcl :: Prelude.Maybe S3ObjectCannedAcl
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'S3DestinationAccessControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cannedAcl', 's3DestinationAccessControl_cannedAcl' - Choose an Amazon S3 canned ACL for MediaConvert to apply to this output.
newS3DestinationAccessControl ::
  S3DestinationAccessControl
newS3DestinationAccessControl =
  S3DestinationAccessControl'
    { cannedAcl =
        Prelude.Nothing
    }

-- | Choose an Amazon S3 canned ACL for MediaConvert to apply to this output.
s3DestinationAccessControl_cannedAcl :: Lens.Lens' S3DestinationAccessControl (Prelude.Maybe S3ObjectCannedAcl)
s3DestinationAccessControl_cannedAcl = Lens.lens (\S3DestinationAccessControl' {cannedAcl} -> cannedAcl) (\s@S3DestinationAccessControl' {} a -> s {cannedAcl = a} :: S3DestinationAccessControl)

instance Prelude.FromJSON S3DestinationAccessControl where
  parseJSON =
    Prelude.withObject
      "S3DestinationAccessControl"
      ( \x ->
          S3DestinationAccessControl'
            Prelude.<$> (x Prelude..:? "cannedAcl")
      )

instance Prelude.Hashable S3DestinationAccessControl

instance Prelude.NFData S3DestinationAccessControl

instance Prelude.ToJSON S3DestinationAccessControl where
  toJSON S3DestinationAccessControl' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("cannedAcl" Prelude..=) Prelude.<$> cannedAcl]
      )
