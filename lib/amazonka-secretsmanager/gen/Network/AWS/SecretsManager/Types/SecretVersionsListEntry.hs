{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types.SecretVersionsListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SecretsManager.Types.SecretVersionsListEntry
  ( SecretVersionsListEntry (..)
  -- * Smart constructor
  , mkSecretVersionsListEntry
  -- * Lenses
  , svleCreatedDate
  , svleLastAccessedDate
  , svleVersionId
  , svleVersionStages
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SecretsManager.Types.SecretVersionIdType as Types
import qualified Network.AWS.SecretsManager.Types.SecretVersionStageType as Types

-- | A structure that contains information about one version of a secret.
--
-- /See:/ 'mkSecretVersionsListEntry' smart constructor.
data SecretVersionsListEntry = SecretVersionsListEntry'
  { createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time this version of the secret was created.
  , lastAccessedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that this version of the secret was last accessed. Note that the resolution of this field is at the date level and does not include the time.
  , versionId :: Core.Maybe Types.SecretVersionIdType
    -- ^ The unique version identifier of this version of the secret.
  , versionStages :: Core.Maybe (Core.NonEmpty Types.SecretVersionStageType)
    -- ^ An array of staging labels that are currently associated with this version of the secret.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SecretVersionsListEntry' value with any optional fields omitted.
mkSecretVersionsListEntry
    :: SecretVersionsListEntry
mkSecretVersionsListEntry
  = SecretVersionsListEntry'{createdDate = Core.Nothing,
                             lastAccessedDate = Core.Nothing, versionId = Core.Nothing,
                             versionStages = Core.Nothing}

-- | The date and time this version of the secret was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svleCreatedDate :: Lens.Lens' SecretVersionsListEntry (Core.Maybe Core.NominalDiffTime)
svleCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE svleCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | The date that this version of the secret was last accessed. Note that the resolution of this field is at the date level and does not include the time.
--
-- /Note:/ Consider using 'lastAccessedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svleLastAccessedDate :: Lens.Lens' SecretVersionsListEntry (Core.Maybe Core.NominalDiffTime)
svleLastAccessedDate = Lens.field @"lastAccessedDate"
{-# INLINEABLE svleLastAccessedDate #-}
{-# DEPRECATED lastAccessedDate "Use generic-lens or generic-optics with 'lastAccessedDate' instead"  #-}

-- | The unique version identifier of this version of the secret.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svleVersionId :: Lens.Lens' SecretVersionsListEntry (Core.Maybe Types.SecretVersionIdType)
svleVersionId = Lens.field @"versionId"
{-# INLINEABLE svleVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

-- | An array of staging labels that are currently associated with this version of the secret.
--
-- /Note:/ Consider using 'versionStages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svleVersionStages :: Lens.Lens' SecretVersionsListEntry (Core.Maybe (Core.NonEmpty Types.SecretVersionStageType))
svleVersionStages = Lens.field @"versionStages"
{-# INLINEABLE svleVersionStages #-}
{-# DEPRECATED versionStages "Use generic-lens or generic-optics with 'versionStages' instead"  #-}

instance Core.FromJSON SecretVersionsListEntry where
        parseJSON
          = Core.withObject "SecretVersionsListEntry" Core.$
              \ x ->
                SecretVersionsListEntry' Core.<$>
                  (x Core..:? "CreatedDate") Core.<*> x Core..:? "LastAccessedDate"
                    Core.<*> x Core..:? "VersionId"
                    Core.<*> x Core..:? "VersionStages"
