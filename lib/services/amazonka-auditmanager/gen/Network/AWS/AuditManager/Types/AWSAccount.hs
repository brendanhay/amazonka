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
-- Module      : Network.AWS.AuditManager.Types.AWSAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AuditManager.Types.AWSAccount where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The wrapper of Amazon Web Services account details, such as account ID,
-- email address, and so on.
--
-- /See:/ 'newAWSAccount' smart constructor.
data AWSAccount = AWSAccount'
  { -- | The name of the specified Amazon Web Services account.
    name :: Prelude.Maybe Prelude.Text,
    -- | The email address associated with the specified Amazon Web Services
    -- account.
    emailAddress :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the specified Amazon Web Services account.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AWSAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'aWSAccount_name' - The name of the specified Amazon Web Services account.
--
-- 'emailAddress', 'aWSAccount_emailAddress' - The email address associated with the specified Amazon Web Services
-- account.
--
-- 'id', 'aWSAccount_id' - The identifier for the specified Amazon Web Services account.
newAWSAccount ::
  AWSAccount
newAWSAccount =
  AWSAccount'
    { name = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The name of the specified Amazon Web Services account.
aWSAccount_name :: Lens.Lens' AWSAccount (Prelude.Maybe Prelude.Text)
aWSAccount_name = Lens.lens (\AWSAccount' {name} -> name) (\s@AWSAccount' {} a -> s {name = a} :: AWSAccount)

-- | The email address associated with the specified Amazon Web Services
-- account.
aWSAccount_emailAddress :: Lens.Lens' AWSAccount (Prelude.Maybe Prelude.Text)
aWSAccount_emailAddress = Lens.lens (\AWSAccount' {emailAddress} -> emailAddress) (\s@AWSAccount' {} a -> s {emailAddress = a} :: AWSAccount)

-- | The identifier for the specified Amazon Web Services account.
aWSAccount_id :: Lens.Lens' AWSAccount (Prelude.Maybe Prelude.Text)
aWSAccount_id = Lens.lens (\AWSAccount' {id} -> id) (\s@AWSAccount' {} a -> s {id = a} :: AWSAccount)

instance Core.FromJSON AWSAccount where
  parseJSON =
    Core.withObject
      "AWSAccount"
      ( \x ->
          AWSAccount'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "emailAddress")
            Prelude.<*> (x Core..:? "id")
      )

instance Prelude.Hashable AWSAccount

instance Prelude.NFData AWSAccount

instance Core.ToJSON AWSAccount where
  toJSON AWSAccount' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("emailAddress" Core..=) Prelude.<$> emailAddress,
            ("id" Core..=) Prelude.<$> id
          ]
      )
