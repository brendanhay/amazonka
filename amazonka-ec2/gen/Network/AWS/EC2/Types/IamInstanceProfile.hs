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
-- Module      : Network.AWS.EC2.Types.IamInstanceProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IamInstanceProfile where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an IAM instance profile.
--
-- /See:/ 'newIamInstanceProfile' smart constructor.
data IamInstanceProfile = IamInstanceProfile'
  { -- | The Amazon Resource Name (ARN) of the instance profile.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the instance profile.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'IamInstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'iamInstanceProfile_arn' - The Amazon Resource Name (ARN) of the instance profile.
--
-- 'id', 'iamInstanceProfile_id' - The ID of the instance profile.
newIamInstanceProfile ::
  IamInstanceProfile
newIamInstanceProfile =
  IamInstanceProfile'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
iamInstanceProfile_arn :: Lens.Lens' IamInstanceProfile (Prelude.Maybe Prelude.Text)
iamInstanceProfile_arn = Lens.lens (\IamInstanceProfile' {arn} -> arn) (\s@IamInstanceProfile' {} a -> s {arn = a} :: IamInstanceProfile)

-- | The ID of the instance profile.
iamInstanceProfile_id :: Lens.Lens' IamInstanceProfile (Prelude.Maybe Prelude.Text)
iamInstanceProfile_id = Lens.lens (\IamInstanceProfile' {id} -> id) (\s@IamInstanceProfile' {} a -> s {id = a} :: IamInstanceProfile)

instance Prelude.FromXML IamInstanceProfile where
  parseXML x =
    IamInstanceProfile'
      Prelude.<$> (x Prelude..@? "arn")
      Prelude.<*> (x Prelude..@? "id")

instance Prelude.Hashable IamInstanceProfile

instance Prelude.NFData IamInstanceProfile
