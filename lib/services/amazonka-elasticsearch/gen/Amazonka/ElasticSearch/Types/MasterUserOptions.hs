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
-- Module      : Amazonka.ElasticSearch.Types.MasterUserOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.MasterUserOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Credentials for the master user: username and password, ARN, or both.
--
-- /See:/ 'newMasterUserOptions' smart constructor.
data MasterUserOptions = MasterUserOptions'
  { -- | ARN for the master user (if IAM is enabled).
    masterUserARN :: Prelude.Maybe Prelude.Text,
    -- | The master user\'s username, which is stored in the Amazon Elasticsearch
    -- Service domain\'s internal database.
    masterUserName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The master user\'s password, which is stored in the Amazon Elasticsearch
    -- Service domain\'s internal database.
    masterUserPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MasterUserOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'masterUserARN', 'masterUserOptions_masterUserARN' - ARN for the master user (if IAM is enabled).
--
-- 'masterUserName', 'masterUserOptions_masterUserName' - The master user\'s username, which is stored in the Amazon Elasticsearch
-- Service domain\'s internal database.
--
-- 'masterUserPassword', 'masterUserOptions_masterUserPassword' - The master user\'s password, which is stored in the Amazon Elasticsearch
-- Service domain\'s internal database.
newMasterUserOptions ::
  MasterUserOptions
newMasterUserOptions =
  MasterUserOptions'
    { masterUserARN = Prelude.Nothing,
      masterUserName = Prelude.Nothing,
      masterUserPassword = Prelude.Nothing
    }

-- | ARN for the master user (if IAM is enabled).
masterUserOptions_masterUserARN :: Lens.Lens' MasterUserOptions (Prelude.Maybe Prelude.Text)
masterUserOptions_masterUserARN = Lens.lens (\MasterUserOptions' {masterUserARN} -> masterUserARN) (\s@MasterUserOptions' {} a -> s {masterUserARN = a} :: MasterUserOptions)

-- | The master user\'s username, which is stored in the Amazon Elasticsearch
-- Service domain\'s internal database.
masterUserOptions_masterUserName :: Lens.Lens' MasterUserOptions (Prelude.Maybe Prelude.Text)
masterUserOptions_masterUserName = Lens.lens (\MasterUserOptions' {masterUserName} -> masterUserName) (\s@MasterUserOptions' {} a -> s {masterUserName = a} :: MasterUserOptions) Prelude.. Lens.mapping Data._Sensitive

-- | The master user\'s password, which is stored in the Amazon Elasticsearch
-- Service domain\'s internal database.
masterUserOptions_masterUserPassword :: Lens.Lens' MasterUserOptions (Prelude.Maybe Prelude.Text)
masterUserOptions_masterUserPassword = Lens.lens (\MasterUserOptions' {masterUserPassword} -> masterUserPassword) (\s@MasterUserOptions' {} a -> s {masterUserPassword = a} :: MasterUserOptions) Prelude.. Lens.mapping Data._Sensitive

instance Prelude.Hashable MasterUserOptions where
  hashWithSalt _salt MasterUserOptions' {..} =
    _salt
      `Prelude.hashWithSalt` masterUserARN
      `Prelude.hashWithSalt` masterUserName
      `Prelude.hashWithSalt` masterUserPassword

instance Prelude.NFData MasterUserOptions where
  rnf MasterUserOptions' {..} =
    Prelude.rnf masterUserARN
      `Prelude.seq` Prelude.rnf masterUserName
      `Prelude.seq` Prelude.rnf masterUserPassword

instance Data.ToJSON MasterUserOptions where
  toJSON MasterUserOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MasterUserARN" Data..=) Prelude.<$> masterUserARN,
            ("MasterUserName" Data..=)
              Prelude.<$> masterUserName,
            ("MasterUserPassword" Data..=)
              Prelude.<$> masterUserPassword
          ]
      )
