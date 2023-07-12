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
-- Module      : Amazonka.AuditManager.Types.AWSAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.AWSAccount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The wrapper of Amazon Web Services account details, such as account ID
-- or email address.
--
-- /See:/ 'newAWSAccount' smart constructor.
data AWSAccount = AWSAccount'
  { -- | The email address that\'s associated with the Amazon Web Services
    -- account.
    emailAddress :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the Amazon Web Services account.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Web Services account.
    name :: Prelude.Maybe Prelude.Text
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
-- 'emailAddress', 'aWSAccount_emailAddress' - The email address that\'s associated with the Amazon Web Services
-- account.
--
-- 'id', 'aWSAccount_id' - The identifier for the Amazon Web Services account.
--
-- 'name', 'aWSAccount_name' - The name of the Amazon Web Services account.
newAWSAccount ::
  AWSAccount
newAWSAccount =
  AWSAccount'
    { emailAddress = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The email address that\'s associated with the Amazon Web Services
-- account.
aWSAccount_emailAddress :: Lens.Lens' AWSAccount (Prelude.Maybe Prelude.Text)
aWSAccount_emailAddress = Lens.lens (\AWSAccount' {emailAddress} -> emailAddress) (\s@AWSAccount' {} a -> s {emailAddress = a} :: AWSAccount)

-- | The identifier for the Amazon Web Services account.
aWSAccount_id :: Lens.Lens' AWSAccount (Prelude.Maybe Prelude.Text)
aWSAccount_id = Lens.lens (\AWSAccount' {id} -> id) (\s@AWSAccount' {} a -> s {id = a} :: AWSAccount)

-- | The name of the Amazon Web Services account.
aWSAccount_name :: Lens.Lens' AWSAccount (Prelude.Maybe Prelude.Text)
aWSAccount_name = Lens.lens (\AWSAccount' {name} -> name) (\s@AWSAccount' {} a -> s {name = a} :: AWSAccount)

instance Data.FromJSON AWSAccount where
  parseJSON =
    Data.withObject
      "AWSAccount"
      ( \x ->
          AWSAccount'
            Prelude.<$> (x Data..:? "emailAddress")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable AWSAccount where
  hashWithSalt _salt AWSAccount' {..} =
    _salt
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData AWSAccount where
  rnf AWSAccount' {..} =
    Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON AWSAccount where
  toJSON AWSAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("emailAddress" Data..=) Prelude.<$> emailAddress,
            ("id" Data..=) Prelude.<$> id,
            ("name" Data..=) Prelude.<$> name
          ]
      )
