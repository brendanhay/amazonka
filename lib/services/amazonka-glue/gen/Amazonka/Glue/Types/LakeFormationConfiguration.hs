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
  { -- | Required for cross account crawls. For same account crawls as the target
    -- data, this can be left as null.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to use Lake Formation credentials for the crawler
    -- instead of the IAM role credentials.
    useLakeFormationCredentials :: Prelude.Maybe Prelude.Bool
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
-- 'accountId', 'lakeFormationConfiguration_accountId' - Required for cross account crawls. For same account crawls as the target
-- data, this can be left as null.
--
-- 'useLakeFormationCredentials', 'lakeFormationConfiguration_useLakeFormationCredentials' - Specifies whether to use Lake Formation credentials for the crawler
-- instead of the IAM role credentials.
newLakeFormationConfiguration ::
  LakeFormationConfiguration
newLakeFormationConfiguration =
  LakeFormationConfiguration'
    { accountId =
        Prelude.Nothing,
      useLakeFormationCredentials = Prelude.Nothing
    }

-- | Required for cross account crawls. For same account crawls as the target
-- data, this can be left as null.
lakeFormationConfiguration_accountId :: Lens.Lens' LakeFormationConfiguration (Prelude.Maybe Prelude.Text)
lakeFormationConfiguration_accountId = Lens.lens (\LakeFormationConfiguration' {accountId} -> accountId) (\s@LakeFormationConfiguration' {} a -> s {accountId = a} :: LakeFormationConfiguration)

-- | Specifies whether to use Lake Formation credentials for the crawler
-- instead of the IAM role credentials.
lakeFormationConfiguration_useLakeFormationCredentials :: Lens.Lens' LakeFormationConfiguration (Prelude.Maybe Prelude.Bool)
lakeFormationConfiguration_useLakeFormationCredentials = Lens.lens (\LakeFormationConfiguration' {useLakeFormationCredentials} -> useLakeFormationCredentials) (\s@LakeFormationConfiguration' {} a -> s {useLakeFormationCredentials = a} :: LakeFormationConfiguration)

instance Data.FromJSON LakeFormationConfiguration where
  parseJSON =
    Data.withObject
      "LakeFormationConfiguration"
      ( \x ->
          LakeFormationConfiguration'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "UseLakeFormationCredentials")
      )

instance Prelude.Hashable LakeFormationConfiguration where
  hashWithSalt _salt LakeFormationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` useLakeFormationCredentials

instance Prelude.NFData LakeFormationConfiguration where
  rnf LakeFormationConfiguration' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf useLakeFormationCredentials

instance Data.ToJSON LakeFormationConfiguration where
  toJSON LakeFormationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            ("UseLakeFormationCredentials" Data..=)
              Prelude.<$> useLakeFormationCredentials
          ]
      )
