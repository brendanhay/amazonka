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
-- Module      : Network.AWS.ElasticSearch.Types.MasterUserOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.MasterUserOptions where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Credentials for the master user: username and password, ARN, or both.
--
-- /See:/ 'newMasterUserOptions' smart constructor.
data MasterUserOptions = MasterUserOptions'
  { -- | The master user\'s password, which is stored in the Amazon Elasticsearch
    -- Service domain\'s internal database.
    masterUserPassword :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The master user\'s username, which is stored in the Amazon Elasticsearch
    -- Service domain\'s internal database.
    masterUserName :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | ARN for the master user (if IAM is enabled).
    masterUserARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MasterUserOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'masterUserPassword', 'masterUserOptions_masterUserPassword' - The master user\'s password, which is stored in the Amazon Elasticsearch
-- Service domain\'s internal database.
--
-- 'masterUserName', 'masterUserOptions_masterUserName' - The master user\'s username, which is stored in the Amazon Elasticsearch
-- Service domain\'s internal database.
--
-- 'masterUserARN', 'masterUserOptions_masterUserARN' - ARN for the master user (if IAM is enabled).
newMasterUserOptions ::
  MasterUserOptions
newMasterUserOptions =
  MasterUserOptions'
    { masterUserPassword =
        Prelude.Nothing,
      masterUserName = Prelude.Nothing,
      masterUserARN = Prelude.Nothing
    }

-- | The master user\'s password, which is stored in the Amazon Elasticsearch
-- Service domain\'s internal database.
masterUserOptions_masterUserPassword :: Lens.Lens' MasterUserOptions (Prelude.Maybe Prelude.Text)
masterUserOptions_masterUserPassword = Lens.lens (\MasterUserOptions' {masterUserPassword} -> masterUserPassword) (\s@MasterUserOptions' {} a -> s {masterUserPassword = a} :: MasterUserOptions) Prelude.. Lens.mapping Prelude._Sensitive

-- | The master user\'s username, which is stored in the Amazon Elasticsearch
-- Service domain\'s internal database.
masterUserOptions_masterUserName :: Lens.Lens' MasterUserOptions (Prelude.Maybe Prelude.Text)
masterUserOptions_masterUserName = Lens.lens (\MasterUserOptions' {masterUserName} -> masterUserName) (\s@MasterUserOptions' {} a -> s {masterUserName = a} :: MasterUserOptions) Prelude.. Lens.mapping Prelude._Sensitive

-- | ARN for the master user (if IAM is enabled).
masterUserOptions_masterUserARN :: Lens.Lens' MasterUserOptions (Prelude.Maybe Prelude.Text)
masterUserOptions_masterUserARN = Lens.lens (\MasterUserOptions' {masterUserARN} -> masterUserARN) (\s@MasterUserOptions' {} a -> s {masterUserARN = a} :: MasterUserOptions)

instance Prelude.Hashable MasterUserOptions

instance Prelude.NFData MasterUserOptions

instance Prelude.ToJSON MasterUserOptions where
  toJSON MasterUserOptions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MasterUserPassword" Prelude..=)
              Prelude.<$> masterUserPassword,
            ("MasterUserName" Prelude..=)
              Prelude.<$> masterUserName,
            ("MasterUserARN" Prelude..=)
              Prelude.<$> masterUserARN
          ]
      )
