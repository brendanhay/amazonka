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
-- Module      : Amazonka.MediaConvert.Types.S3DestinationAccessControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.S3DestinationAccessControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.S3ObjectCannedAcl
import qualified Amazonka.Prelude as Prelude

-- | Optional. Have MediaConvert automatically apply Amazon S3 access control
-- for the outputs in this output group. When you don\'t use this setting,
-- S3 automatically applies the default access control list PRIVATE.
--
-- /See:/ 'newS3DestinationAccessControl' smart constructor.
data S3DestinationAccessControl = S3DestinationAccessControl'
  { -- | Choose an Amazon S3 canned ACL for MediaConvert to apply to this output.
    cannedAcl :: Prelude.Maybe S3ObjectCannedAcl
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON S3DestinationAccessControl where
  parseJSON =
    Data.withObject
      "S3DestinationAccessControl"
      ( \x ->
          S3DestinationAccessControl'
            Prelude.<$> (x Data..:? "cannedAcl")
      )

instance Prelude.Hashable S3DestinationAccessControl where
  hashWithSalt _salt S3DestinationAccessControl' {..} =
    _salt `Prelude.hashWithSalt` cannedAcl

instance Prelude.NFData S3DestinationAccessControl where
  rnf S3DestinationAccessControl' {..} =
    Prelude.rnf cannedAcl

instance Data.ToJSON S3DestinationAccessControl where
  toJSON S3DestinationAccessControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [("cannedAcl" Data..=) Prelude.<$> cannedAcl]
      )
