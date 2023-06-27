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
-- Module      : Amazonka.PaymentCryptography.Types.Alias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.Alias where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an alias.
--
-- /See:/ 'newAlias' smart constructor.
data Alias = Alias'
  { -- | The @KeyARN@ of the key associated with the alias.
    keyArn :: Prelude.Maybe Prelude.Text,
    -- | A friendly name that you can use to refer to a key. The value must begin
    -- with @alias\/@.
    --
    -- Do not include confidential or sensitive information in this field. This
    -- field may be displayed in plaintext in CloudTrail logs and other output.
    aliasName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Alias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyArn', 'alias_keyArn' - The @KeyARN@ of the key associated with the alias.
--
-- 'aliasName', 'alias_aliasName' - A friendly name that you can use to refer to a key. The value must begin
-- with @alias\/@.
--
-- Do not include confidential or sensitive information in this field. This
-- field may be displayed in plaintext in CloudTrail logs and other output.
newAlias ::
  -- | 'aliasName'
  Prelude.Text ->
  Alias
newAlias pAliasName_ =
  Alias'
    { keyArn = Prelude.Nothing,
      aliasName = pAliasName_
    }

-- | The @KeyARN@ of the key associated with the alias.
alias_keyArn :: Lens.Lens' Alias (Prelude.Maybe Prelude.Text)
alias_keyArn = Lens.lens (\Alias' {keyArn} -> keyArn) (\s@Alias' {} a -> s {keyArn = a} :: Alias)

-- | A friendly name that you can use to refer to a key. The value must begin
-- with @alias\/@.
--
-- Do not include confidential or sensitive information in this field. This
-- field may be displayed in plaintext in CloudTrail logs and other output.
alias_aliasName :: Lens.Lens' Alias Prelude.Text
alias_aliasName = Lens.lens (\Alias' {aliasName} -> aliasName) (\s@Alias' {} a -> s {aliasName = a} :: Alias)

instance Data.FromJSON Alias where
  parseJSON =
    Data.withObject
      "Alias"
      ( \x ->
          Alias'
            Prelude.<$> (x Data..:? "KeyArn")
            Prelude.<*> (x Data..: "AliasName")
      )

instance Prelude.Hashable Alias where
  hashWithSalt _salt Alias' {..} =
    _salt
      `Prelude.hashWithSalt` keyArn
      `Prelude.hashWithSalt` aliasName

instance Prelude.NFData Alias where
  rnf Alias' {..} =
    Prelude.rnf keyArn
      `Prelude.seq` Prelude.rnf aliasName
