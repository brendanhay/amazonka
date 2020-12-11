-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.IdentityPoolUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.IdentityPoolUsage
  ( IdentityPoolUsage (..),

    -- * Smart constructor
    mkIdentityPoolUsage,

    -- * Lenses
    ipuLastModifiedDate,
    ipuIdentityPoolId,
    ipuDataStorage,
    ipuSyncSessionsCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Usage information for the identity pool.
--
-- /See:/ 'mkIdentityPoolUsage' smart constructor.
data IdentityPoolUsage = IdentityPoolUsage'
  { lastModifiedDate ::
      Lude.Maybe Lude.Timestamp,
    identityPoolId :: Lude.Maybe Lude.Text,
    dataStorage :: Lude.Maybe Lude.Integer,
    syncSessionsCount :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IdentityPoolUsage' with the minimum fields required to make a request.
--
-- * 'dataStorage' - Data storage information for the identity pool.
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
-- * 'lastModifiedDate' - Date on which the identity pool was last modified.
-- * 'syncSessionsCount' - Number of sync sessions for the identity pool.
mkIdentityPoolUsage ::
  IdentityPoolUsage
mkIdentityPoolUsage =
  IdentityPoolUsage'
    { lastModifiedDate = Lude.Nothing,
      identityPoolId = Lude.Nothing,
      dataStorage = Lude.Nothing,
      syncSessionsCount = Lude.Nothing
    }

-- | Date on which the identity pool was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipuLastModifiedDate :: Lens.Lens' IdentityPoolUsage (Lude.Maybe Lude.Timestamp)
ipuLastModifiedDate = Lens.lens (lastModifiedDate :: IdentityPoolUsage -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: IdentityPoolUsage)
{-# DEPRECATED ipuLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipuIdentityPoolId :: Lens.Lens' IdentityPoolUsage (Lude.Maybe Lude.Text)
ipuIdentityPoolId = Lens.lens (identityPoolId :: IdentityPoolUsage -> Lude.Maybe Lude.Text) (\s a -> s {identityPoolId = a} :: IdentityPoolUsage)
{-# DEPRECATED ipuIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | Data storage information for the identity pool.
--
-- /Note:/ Consider using 'dataStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipuDataStorage :: Lens.Lens' IdentityPoolUsage (Lude.Maybe Lude.Integer)
ipuDataStorage = Lens.lens (dataStorage :: IdentityPoolUsage -> Lude.Maybe Lude.Integer) (\s a -> s {dataStorage = a} :: IdentityPoolUsage)
{-# DEPRECATED ipuDataStorage "Use generic-lens or generic-optics with 'dataStorage' instead." #-}

-- | Number of sync sessions for the identity pool.
--
-- /Note:/ Consider using 'syncSessionsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipuSyncSessionsCount :: Lens.Lens' IdentityPoolUsage (Lude.Maybe Lude.Integer)
ipuSyncSessionsCount = Lens.lens (syncSessionsCount :: IdentityPoolUsage -> Lude.Maybe Lude.Integer) (\s a -> s {syncSessionsCount = a} :: IdentityPoolUsage)
{-# DEPRECATED ipuSyncSessionsCount "Use generic-lens or generic-optics with 'syncSessionsCount' instead." #-}

instance Lude.FromJSON IdentityPoolUsage where
  parseJSON =
    Lude.withObject
      "IdentityPoolUsage"
      ( \x ->
          IdentityPoolUsage'
            Lude.<$> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "IdentityPoolId")
            Lude.<*> (x Lude..:? "DataStorage")
            Lude.<*> (x Lude..:? "SyncSessionsCount")
      )
