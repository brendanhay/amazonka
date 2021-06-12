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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Credentials for the master user: username and password, ARN, or both.
--
-- /See:/ 'newMasterUserOptions' smart constructor.
data MasterUserOptions = MasterUserOptions'
  { -- | The master user\'s password, which is stored in the Amazon Elasticsearch
    -- Service domain\'s internal database.
    masterUserPassword :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The master user\'s username, which is stored in the Amazon Elasticsearch
    -- Service domain\'s internal database.
    masterUserName :: Core.Maybe (Core.Sensitive Core.Text),
    -- | ARN for the master user (if IAM is enabled).
    masterUserARN :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
        Core.Nothing,
      masterUserName = Core.Nothing,
      masterUserARN = Core.Nothing
    }

-- | The master user\'s password, which is stored in the Amazon Elasticsearch
-- Service domain\'s internal database.
masterUserOptions_masterUserPassword :: Lens.Lens' MasterUserOptions (Core.Maybe Core.Text)
masterUserOptions_masterUserPassword = Lens.lens (\MasterUserOptions' {masterUserPassword} -> masterUserPassword) (\s@MasterUserOptions' {} a -> s {masterUserPassword = a} :: MasterUserOptions) Core.. Lens.mapping Core._Sensitive

-- | The master user\'s username, which is stored in the Amazon Elasticsearch
-- Service domain\'s internal database.
masterUserOptions_masterUserName :: Lens.Lens' MasterUserOptions (Core.Maybe Core.Text)
masterUserOptions_masterUserName = Lens.lens (\MasterUserOptions' {masterUserName} -> masterUserName) (\s@MasterUserOptions' {} a -> s {masterUserName = a} :: MasterUserOptions) Core.. Lens.mapping Core._Sensitive

-- | ARN for the master user (if IAM is enabled).
masterUserOptions_masterUserARN :: Lens.Lens' MasterUserOptions (Core.Maybe Core.Text)
masterUserOptions_masterUserARN = Lens.lens (\MasterUserOptions' {masterUserARN} -> masterUserARN) (\s@MasterUserOptions' {} a -> s {masterUserARN = a} :: MasterUserOptions)

instance Core.Hashable MasterUserOptions

instance Core.NFData MasterUserOptions

instance Core.ToJSON MasterUserOptions where
  toJSON MasterUserOptions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MasterUserPassword" Core..=)
              Core.<$> masterUserPassword,
            ("MasterUserName" Core..=) Core.<$> masterUserName,
            ("MasterUserARN" Core..=) Core.<$> masterUserARN
          ]
      )
