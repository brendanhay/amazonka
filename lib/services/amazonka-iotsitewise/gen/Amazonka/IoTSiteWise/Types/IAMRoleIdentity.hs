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
-- Module      : Amazonka.IoTSiteWise.Types.IAMRoleIdentity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.IAMRoleIdentity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an Identity and Access Management role. For
-- more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM roles>
-- in the /IAM User Guide/.
--
-- /See:/ 'newIAMRoleIdentity' smart constructor.
data IAMRoleIdentity = IAMRoleIdentity'
  { -- | The ARN of the IAM role. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM ARNs>
    -- in the /IAM User Guide/.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IAMRoleIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'iAMRoleIdentity_arn' - The ARN of the IAM role. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM ARNs>
-- in the /IAM User Guide/.
newIAMRoleIdentity ::
  -- | 'arn'
  Prelude.Text ->
  IAMRoleIdentity
newIAMRoleIdentity pArn_ =
  IAMRoleIdentity' {arn = pArn_}

-- | The ARN of the IAM role. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM ARNs>
-- in the /IAM User Guide/.
iAMRoleIdentity_arn :: Lens.Lens' IAMRoleIdentity Prelude.Text
iAMRoleIdentity_arn = Lens.lens (\IAMRoleIdentity' {arn} -> arn) (\s@IAMRoleIdentity' {} a -> s {arn = a} :: IAMRoleIdentity)

instance Core.FromJSON IAMRoleIdentity where
  parseJSON =
    Core.withObject
      "IAMRoleIdentity"
      ( \x ->
          IAMRoleIdentity' Prelude.<$> (x Core..: "arn")
      )

instance Prelude.Hashable IAMRoleIdentity where
  hashWithSalt _salt IAMRoleIdentity' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData IAMRoleIdentity where
  rnf IAMRoleIdentity' {..} = Prelude.rnf arn

instance Core.ToJSON IAMRoleIdentity where
  toJSON IAMRoleIdentity' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Core..= arn)]
      )
