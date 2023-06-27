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
-- Module      : Amazonka.IoT.Types.SecurityProfileIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.SecurityProfileIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifying information for a Device Defender security profile.
--
-- /See:/ 'newSecurityProfileIdentifier' smart constructor.
data SecurityProfileIdentifier = SecurityProfileIdentifier'
  { -- | The name you\'ve given to the security profile.
    name :: Prelude.Text,
    -- | The ARN of the security profile.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON SecurityProfileIdentifier where
  parseJSON =
    Data.withObject
      "SecurityProfileIdentifier"
      ( \x ->
          SecurityProfileIdentifier'
            Prelude.<$> (x Data..: "name")
            Prelude.<*> (x Data..: "arn")
      )

instance Prelude.Hashable SecurityProfileIdentifier where
  hashWithSalt _salt SecurityProfileIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn

instance Prelude.NFData SecurityProfileIdentifier where
  rnf SecurityProfileIdentifier' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf arn
