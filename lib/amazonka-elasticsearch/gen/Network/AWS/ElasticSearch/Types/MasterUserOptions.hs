{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.MasterUserOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.MasterUserOptions
  ( MasterUserOptions (..),

    -- * Smart constructor
    mkMasterUserOptions,

    -- * Lenses
    muoMasterUserARN,
    muoMasterUserName,
    muoMasterUserPassword,
  )
where

import qualified Network.AWS.ElasticSearch.Types.ARN as Types
import qualified Network.AWS.ElasticSearch.Types.Password as Types
import qualified Network.AWS.ElasticSearch.Types.Username as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Credentials for the master user: username and password, ARN, or both.
--
-- /See:/ 'mkMasterUserOptions' smart constructor.
data MasterUserOptions = MasterUserOptions'
  { -- | ARN for the master user (if IAM is enabled).
    masterUserARN :: Core.Maybe Types.ARN,
    -- | The master user's username, which is stored in the Amazon Elasticsearch Service domain's internal database.
    masterUserName :: Core.Maybe Types.Username,
    -- | The master user's password, which is stored in the Amazon Elasticsearch Service domain's internal database.
    masterUserPassword :: Core.Maybe Types.Password
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MasterUserOptions' value with any optional fields omitted.
mkMasterUserOptions ::
  MasterUserOptions
mkMasterUserOptions =
  MasterUserOptions'
    { masterUserARN = Core.Nothing,
      masterUserName = Core.Nothing,
      masterUserPassword = Core.Nothing
    }

-- | ARN for the master user (if IAM is enabled).
--
-- /Note:/ Consider using 'masterUserARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muoMasterUserARN :: Lens.Lens' MasterUserOptions (Core.Maybe Types.ARN)
muoMasterUserARN = Lens.field @"masterUserARN"
{-# DEPRECATED muoMasterUserARN "Use generic-lens or generic-optics with 'masterUserARN' instead." #-}

-- | The master user's username, which is stored in the Amazon Elasticsearch Service domain's internal database.
--
-- /Note:/ Consider using 'masterUserName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muoMasterUserName :: Lens.Lens' MasterUserOptions (Core.Maybe Types.Username)
muoMasterUserName = Lens.field @"masterUserName"
{-# DEPRECATED muoMasterUserName "Use generic-lens or generic-optics with 'masterUserName' instead." #-}

-- | The master user's password, which is stored in the Amazon Elasticsearch Service domain's internal database.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muoMasterUserPassword :: Lens.Lens' MasterUserOptions (Core.Maybe Types.Password)
muoMasterUserPassword = Lens.field @"masterUserPassword"
{-# DEPRECATED muoMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

instance Core.FromJSON MasterUserOptions where
  toJSON MasterUserOptions {..} =
    Core.object
      ( Core.catMaybes
          [ ("MasterUserARN" Core..=) Core.<$> masterUserARN,
            ("MasterUserName" Core..=) Core.<$> masterUserName,
            ("MasterUserPassword" Core..=) Core.<$> masterUserPassword
          ]
      )
