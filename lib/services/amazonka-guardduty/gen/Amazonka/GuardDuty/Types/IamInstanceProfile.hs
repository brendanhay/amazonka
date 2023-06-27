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
-- Module      : Amazonka.GuardDuty.Types.IamInstanceProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.IamInstanceProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the EC2 instance profile.
--
-- /See:/ 'newIamInstanceProfile' smart constructor.
data IamInstanceProfile = IamInstanceProfile'
  { -- | The profile ARN of the EC2 instance.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The profile ID of the EC2 instance.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IamInstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'iamInstanceProfile_arn' - The profile ARN of the EC2 instance.
--
-- 'id', 'iamInstanceProfile_id' - The profile ID of the EC2 instance.
newIamInstanceProfile ::
  IamInstanceProfile
newIamInstanceProfile =
  IamInstanceProfile'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The profile ARN of the EC2 instance.
iamInstanceProfile_arn :: Lens.Lens' IamInstanceProfile (Prelude.Maybe Prelude.Text)
iamInstanceProfile_arn = Lens.lens (\IamInstanceProfile' {arn} -> arn) (\s@IamInstanceProfile' {} a -> s {arn = a} :: IamInstanceProfile)

-- | The profile ID of the EC2 instance.
iamInstanceProfile_id :: Lens.Lens' IamInstanceProfile (Prelude.Maybe Prelude.Text)
iamInstanceProfile_id = Lens.lens (\IamInstanceProfile' {id} -> id) (\s@IamInstanceProfile' {} a -> s {id = a} :: IamInstanceProfile)

instance Data.FromJSON IamInstanceProfile where
  parseJSON =
    Data.withObject
      "IamInstanceProfile"
      ( \x ->
          IamInstanceProfile'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "id")
      )

instance Prelude.Hashable IamInstanceProfile where
  hashWithSalt _salt IamInstanceProfile' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id

instance Prelude.NFData IamInstanceProfile where
  rnf IamInstanceProfile' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf id
