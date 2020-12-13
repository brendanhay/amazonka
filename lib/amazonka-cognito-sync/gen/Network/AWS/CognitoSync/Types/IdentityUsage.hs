{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.IdentityUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.IdentityUsage
  ( IdentityUsage (..),

    -- * Smart constructor
    mkIdentityUsage,

    -- * Lenses
    iuLastModifiedDate,
    iuIdentityPoolId,
    iuDatasetCount,
    iuDataStorage,
    iuIdentityId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Usage information for the identity.
--
-- /See:/ 'mkIdentityUsage' smart constructor.
data IdentityUsage = IdentityUsage'
  { -- | Date on which the identity was last modified.
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityPoolId :: Lude.Maybe Lude.Text,
    -- | Number of datasets for the identity.
    datasetCount :: Lude.Maybe Lude.Int,
    -- | Total data storage for this identity.
    dataStorage :: Lude.Maybe Lude.Integer,
    -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IdentityUsage' with the minimum fields required to make a request.
--
-- * 'lastModifiedDate' - Date on which the identity was last modified.
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
-- * 'datasetCount' - Number of datasets for the identity.
-- * 'dataStorage' - Total data storage for this identity.
-- * 'identityId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
mkIdentityUsage ::
  IdentityUsage
mkIdentityUsage =
  IdentityUsage'
    { lastModifiedDate = Lude.Nothing,
      identityPoolId = Lude.Nothing,
      datasetCount = Lude.Nothing,
      dataStorage = Lude.Nothing,
      identityId = Lude.Nothing
    }

-- | Date on which the identity was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuLastModifiedDate :: Lens.Lens' IdentityUsage (Lude.Maybe Lude.Timestamp)
iuLastModifiedDate = Lens.lens (lastModifiedDate :: IdentityUsage -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: IdentityUsage)
{-# DEPRECATED iuLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuIdentityPoolId :: Lens.Lens' IdentityUsage (Lude.Maybe Lude.Text)
iuIdentityPoolId = Lens.lens (identityPoolId :: IdentityUsage -> Lude.Maybe Lude.Text) (\s a -> s {identityPoolId = a} :: IdentityUsage)
{-# DEPRECATED iuIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | Number of datasets for the identity.
--
-- /Note:/ Consider using 'datasetCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuDatasetCount :: Lens.Lens' IdentityUsage (Lude.Maybe Lude.Int)
iuDatasetCount = Lens.lens (datasetCount :: IdentityUsage -> Lude.Maybe Lude.Int) (\s a -> s {datasetCount = a} :: IdentityUsage)
{-# DEPRECATED iuDatasetCount "Use generic-lens or generic-optics with 'datasetCount' instead." #-}

-- | Total data storage for this identity.
--
-- /Note:/ Consider using 'dataStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuDataStorage :: Lens.Lens' IdentityUsage (Lude.Maybe Lude.Integer)
iuDataStorage = Lens.lens (dataStorage :: IdentityUsage -> Lude.Maybe Lude.Integer) (\s a -> s {dataStorage = a} :: IdentityUsage)
{-# DEPRECATED iuDataStorage "Use generic-lens or generic-optics with 'dataStorage' instead." #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuIdentityId :: Lens.Lens' IdentityUsage (Lude.Maybe Lude.Text)
iuIdentityId = Lens.lens (identityId :: IdentityUsage -> Lude.Maybe Lude.Text) (\s a -> s {identityId = a} :: IdentityUsage)
{-# DEPRECATED iuIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

instance Lude.FromJSON IdentityUsage where
  parseJSON =
    Lude.withObject
      "IdentityUsage"
      ( \x ->
          IdentityUsage'
            Lude.<$> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "IdentityPoolId")
            Lude.<*> (x Lude..:? "DatasetCount")
            Lude.<*> (x Lude..:? "DataStorage")
            Lude.<*> (x Lude..:? "IdentityId")
      )
