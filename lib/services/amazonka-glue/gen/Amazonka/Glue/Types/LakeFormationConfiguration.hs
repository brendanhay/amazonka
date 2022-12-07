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
-- Module      : Amazonka.Glue.Types.LakeFormationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.LakeFormationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies Lake Formation configuration settings for the crawler.
--
-- /See:/ 'newLakeFormationConfiguration' smart constructor.
data LakeFormationConfiguration = LakeFormationConfiguration'
  { -- | Specifies whether to use Lake Formation credentials for the crawler
    -- instead of the IAM role credentials.
    useLakeFormationCredentials :: Prelude.Maybe Prelude.Bool,
    -- | Required for cross account crawls. For same account crawls as the target
    -- data, this can be left as null.
    accountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LakeFormationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'useLakeFormationCredentials', 'lakeFormationConfiguration_useLakeFormationCredentials' - Specifies whether to use Lake Formation credentials for the crawler
-- instead of the IAM role credentials.
--
-- 'accountId', 'lakeFormationConfiguration_accountId' - Required for cross account crawls. For same account crawls as the target
-- data, this can be left as null.
newLakeFormationConfiguration ::
  LakeFormationConfiguration
newLakeFormationConfiguration =
  LakeFormationConfiguration'
    { useLakeFormationCredentials =
        Prelude.Nothing,
      accountId = Prelude.Nothing
    }

-- | Specifies whether to use Lake Formation credentials for the crawler
-- instead of the IAM role credentials.
lakeFormationConfiguration_useLakeFormationCredentials :: Lens.Lens' LakeFormationConfiguration (Prelude.Maybe Prelude.Bool)
lakeFormationConfiguration_useLakeFormationCredentials = Lens.lens (\LakeFormationConfiguration' {useLakeFormationCredentials} -> useLakeFormationCredentials) (\s@LakeFormationConfiguration' {} a -> s {useLakeFormationCredentials = a} :: LakeFormationConfiguration)

-- | Required for cross account crawls. For same account crawls as the target
-- data, this can be left as null.
lakeFormationConfiguration_accountId :: Lens.Lens' LakeFormationConfiguration (Prelude.Maybe Prelude.Text)
lakeFormationConfiguration_accountId = Lens.lens (\LakeFormationConfiguration' {accountId} -> accountId) (\s@LakeFormationConfiguration' {} a -> s {accountId = a} :: LakeFormationConfiguration)

instance Data.FromJSON LakeFormationConfiguration where
  parseJSON =
    Data.withObject
      "LakeFormationConfiguration"
      ( \x ->
          LakeFormationConfiguration'
            Prelude.<$> (x Data..:? "UseLakeFormationCredentials")
            Prelude.<*> (x Data..:? "AccountId")
      )

instance Prelude.Hashable LakeFormationConfiguration where
  hashWithSalt _salt LakeFormationConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` useLakeFormationCredentials
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData LakeFormationConfiguration where
  rnf LakeFormationConfiguration' {..} =
    Prelude.rnf useLakeFormationCredentials
      `Prelude.seq` Prelude.rnf accountId

instance Data.ToJSON LakeFormationConfiguration where
  toJSON LakeFormationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("UseLakeFormationCredentials" Data..=)
              Prelude.<$> useLakeFormationCredentials,
            ("AccountId" Data..=) Prelude.<$> accountId
          ]
      )
