{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.StorageTypeLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.StorageTypeLimit
  ( StorageTypeLimit (..),

    -- * Smart constructor
    mkStorageTypeLimit,

    -- * Lenses
    stlLimitName,
    stlLimitValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Limits that are applicable for given storage type.
--
-- /See:/ 'mkStorageTypeLimit' smart constructor.
data StorageTypeLimit = StorageTypeLimit'
  { -- | Name of storage limits that are applicable for given storage type. If @'StorageType' @ is ebs, following storage options are applicable
    --
    --     * MinimumVolumeSize
    -- Minimum amount of volume size that is applicable for given storage type.It can be empty if it is not applicable.
    --     * MaximumVolumeSize
    -- Maximum amount of volume size that is applicable for given storage type.It can be empty if it is not applicable.
    --     * MaximumIops
    -- Maximum amount of Iops that is applicable for given storage type.It can be empty if it is not applicable.
    --     * MinimumIops
    -- Minimum amount of Iops that is applicable for given storage type.It can be empty if it is not applicable.
    limitName :: Lude.Maybe Lude.Text,
    -- | Values for the @'StorageTypeLimit$LimitName' @ .
    limitValues :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StorageTypeLimit' with the minimum fields required to make a request.
--
-- * 'limitName' - Name of storage limits that are applicable for given storage type. If @'StorageType' @ is ebs, following storage options are applicable
--
--     * MinimumVolumeSize
-- Minimum amount of volume size that is applicable for given storage type.It can be empty if it is not applicable.
--     * MaximumVolumeSize
-- Maximum amount of volume size that is applicable for given storage type.It can be empty if it is not applicable.
--     * MaximumIops
-- Maximum amount of Iops that is applicable for given storage type.It can be empty if it is not applicable.
--     * MinimumIops
-- Minimum amount of Iops that is applicable for given storage type.It can be empty if it is not applicable.
--
-- * 'limitValues' - Values for the @'StorageTypeLimit$LimitName' @ .
mkStorageTypeLimit ::
  StorageTypeLimit
mkStorageTypeLimit =
  StorageTypeLimit'
    { limitName = Lude.Nothing,
      limitValues = Lude.Nothing
    }

-- | Name of storage limits that are applicable for given storage type. If @'StorageType' @ is ebs, following storage options are applicable
--
--     * MinimumVolumeSize
-- Minimum amount of volume size that is applicable for given storage type.It can be empty if it is not applicable.
--     * MaximumVolumeSize
-- Maximum amount of volume size that is applicable for given storage type.It can be empty if it is not applicable.
--     * MaximumIops
-- Maximum amount of Iops that is applicable for given storage type.It can be empty if it is not applicable.
--     * MinimumIops
-- Minimum amount of Iops that is applicable for given storage type.It can be empty if it is not applicable.
--
--
-- /Note:/ Consider using 'limitName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stlLimitName :: Lens.Lens' StorageTypeLimit (Lude.Maybe Lude.Text)
stlLimitName = Lens.lens (limitName :: StorageTypeLimit -> Lude.Maybe Lude.Text) (\s a -> s {limitName = a} :: StorageTypeLimit)
{-# DEPRECATED stlLimitName "Use generic-lens or generic-optics with 'limitName' instead." #-}

-- | Values for the @'StorageTypeLimit$LimitName' @ .
--
-- /Note:/ Consider using 'limitValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stlLimitValues :: Lens.Lens' StorageTypeLimit (Lude.Maybe [Lude.Text])
stlLimitValues = Lens.lens (limitValues :: StorageTypeLimit -> Lude.Maybe [Lude.Text]) (\s a -> s {limitValues = a} :: StorageTypeLimit)
{-# DEPRECATED stlLimitValues "Use generic-lens or generic-optics with 'limitValues' instead." #-}

instance Lude.FromJSON StorageTypeLimit where
  parseJSON =
    Lude.withObject
      "StorageTypeLimit"
      ( \x ->
          StorageTypeLimit'
            Lude.<$> (x Lude..:? "LimitName")
            Lude.<*> (x Lude..:? "LimitValues" Lude..!= Lude.mempty)
      )
