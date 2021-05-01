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
-- Module      : Network.AWS.IoT.Types.SecurityProfileIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SecurityProfileIdentifier where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Identifying information for a Device Defender security profile.
--
-- /See:/ 'newSecurityProfileIdentifier' smart constructor.
data SecurityProfileIdentifier = SecurityProfileIdentifier'
  { -- | The name you\'ve given to the security profile.
    name :: Prelude.Text,
    -- | The ARN of the security profile.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SecurityProfileIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'securityProfileIdentifier_name' - The name you\'ve given to the security profile.
--
-- 'arn', 'securityProfileIdentifier_arn' - The ARN of the security profile.
newSecurityProfileIdentifier ::
  -- | 'name'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  SecurityProfileIdentifier
newSecurityProfileIdentifier pName_ pArn_ =
  SecurityProfileIdentifier'
    { name = pName_,
      arn = pArn_
    }

-- | The name you\'ve given to the security profile.
securityProfileIdentifier_name :: Lens.Lens' SecurityProfileIdentifier Prelude.Text
securityProfileIdentifier_name = Lens.lens (\SecurityProfileIdentifier' {name} -> name) (\s@SecurityProfileIdentifier' {} a -> s {name = a} :: SecurityProfileIdentifier)

-- | The ARN of the security profile.
securityProfileIdentifier_arn :: Lens.Lens' SecurityProfileIdentifier Prelude.Text
securityProfileIdentifier_arn = Lens.lens (\SecurityProfileIdentifier' {arn} -> arn) (\s@SecurityProfileIdentifier' {} a -> s {arn = a} :: SecurityProfileIdentifier)

instance Prelude.FromJSON SecurityProfileIdentifier where
  parseJSON =
    Prelude.withObject
      "SecurityProfileIdentifier"
      ( \x ->
          SecurityProfileIdentifier'
            Prelude.<$> (x Prelude..: "name")
            Prelude.<*> (x Prelude..: "arn")
      )

instance Prelude.Hashable SecurityProfileIdentifier

instance Prelude.NFData SecurityProfileIdentifier
