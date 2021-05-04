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
-- Module      : Network.AWS.Connect.Types.SecurityProfileSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.SecurityProfileSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a security profile.
--
-- /See:/ 'newSecurityProfileSummary' smart constructor.
data SecurityProfileSummary = SecurityProfileSummary'
  { -- | The Amazon Resource Name (ARN) of the security profile.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the security profile.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the security profile.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SecurityProfileSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'securityProfileSummary_arn' - The Amazon Resource Name (ARN) of the security profile.
--
-- 'id', 'securityProfileSummary_id' - The identifier of the security profile.
--
-- 'name', 'securityProfileSummary_name' - The name of the security profile.
newSecurityProfileSummary ::
  SecurityProfileSummary
newSecurityProfileSummary =
  SecurityProfileSummary'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the security profile.
securityProfileSummary_arn :: Lens.Lens' SecurityProfileSummary (Prelude.Maybe Prelude.Text)
securityProfileSummary_arn = Lens.lens (\SecurityProfileSummary' {arn} -> arn) (\s@SecurityProfileSummary' {} a -> s {arn = a} :: SecurityProfileSummary)

-- | The identifier of the security profile.
securityProfileSummary_id :: Lens.Lens' SecurityProfileSummary (Prelude.Maybe Prelude.Text)
securityProfileSummary_id = Lens.lens (\SecurityProfileSummary' {id} -> id) (\s@SecurityProfileSummary' {} a -> s {id = a} :: SecurityProfileSummary)

-- | The name of the security profile.
securityProfileSummary_name :: Lens.Lens' SecurityProfileSummary (Prelude.Maybe Prelude.Text)
securityProfileSummary_name = Lens.lens (\SecurityProfileSummary' {name} -> name) (\s@SecurityProfileSummary' {} a -> s {name = a} :: SecurityProfileSummary)

instance Prelude.FromJSON SecurityProfileSummary where
  parseJSON =
    Prelude.withObject
      "SecurityProfileSummary"
      ( \x ->
          SecurityProfileSummary'
            Prelude.<$> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable SecurityProfileSummary

instance Prelude.NFData SecurityProfileSummary
