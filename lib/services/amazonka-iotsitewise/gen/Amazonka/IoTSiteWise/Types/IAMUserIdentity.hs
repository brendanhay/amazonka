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
-- Module      : Amazonka.IoTSiteWise.Types.IAMUserIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.IAMUserIdentity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an Identity and Access Management user.
--
-- /See:/ 'newIAMUserIdentity' smart constructor.
data IAMUserIdentity = IAMUserIdentity'
  { -- | The ARN of the IAM user. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM ARNs>
    -- in the /IAM User Guide/.
    --
    -- If you delete the IAM user, access policies that contain this identity
    -- include an empty @arn@. You can delete the access policy for the IAM
    -- user that no longer exists.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IAMUserIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'iAMUserIdentity_arn' - The ARN of the IAM user. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM ARNs>
-- in the /IAM User Guide/.
--
-- If you delete the IAM user, access policies that contain this identity
-- include an empty @arn@. You can delete the access policy for the IAM
-- user that no longer exists.
newIAMUserIdentity ::
  -- | 'arn'
  Prelude.Text ->
  IAMUserIdentity
newIAMUserIdentity pArn_ =
  IAMUserIdentity' {arn = pArn_}

-- | The ARN of the IAM user. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM ARNs>
-- in the /IAM User Guide/.
--
-- If you delete the IAM user, access policies that contain this identity
-- include an empty @arn@. You can delete the access policy for the IAM
-- user that no longer exists.
iAMUserIdentity_arn :: Lens.Lens' IAMUserIdentity Prelude.Text
iAMUserIdentity_arn = Lens.lens (\IAMUserIdentity' {arn} -> arn) (\s@IAMUserIdentity' {} a -> s {arn = a} :: IAMUserIdentity)

instance Data.FromJSON IAMUserIdentity where
  parseJSON =
    Data.withObject
      "IAMUserIdentity"
      ( \x ->
          IAMUserIdentity' Prelude.<$> (x Data..: "arn")
      )

instance Prelude.Hashable IAMUserIdentity where
  hashWithSalt _salt IAMUserIdentity' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData IAMUserIdentity where
  rnf IAMUserIdentity' {..} = Prelude.rnf arn

instance Data.ToJSON IAMUserIdentity where
  toJSON IAMUserIdentity' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )
