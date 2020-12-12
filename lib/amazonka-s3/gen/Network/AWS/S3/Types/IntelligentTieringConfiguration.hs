{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.IntelligentTieringConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.IntelligentTieringConfiguration
  ( IntelligentTieringConfiguration (..),

    -- * Smart constructor
    mkIntelligentTieringConfiguration,

    -- * Lenses
    itcFilter,
    itcId,
    itcStatus,
    itcTierings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.IntelligentTieringFilter
import Network.AWS.S3.Types.IntelligentTieringStatus
import Network.AWS.S3.Types.Tiering

-- | Specifies the S3 Intelligent-Tiering configuration for an Amazon S3 bucket.
--
-- For information about the S3 Intelligent-Tiering storage class, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> .
--
-- /See:/ 'mkIntelligentTieringConfiguration' smart constructor.
data IntelligentTieringConfiguration = IntelligentTieringConfiguration'
  { filter ::
      Lude.Maybe
        IntelligentTieringFilter,
    id :: Lude.Text,
    status ::
      IntelligentTieringStatus,
    tierings :: [Tiering]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IntelligentTieringConfiguration' with the minimum fields required to make a request.
--
-- * 'filter' - Specifies a bucket filter. The configuration only includes objects that meet the filter's criteria.
-- * 'id' - The ID used to identify the S3 Intelligent-Tiering configuration.
-- * 'status' - Specifies the status of the configuration.
-- * 'tierings' - Specifies the S3 Intelligent-Tiering storage class tier of the configuration.
mkIntelligentTieringConfiguration ::
  -- | 'id'
  Lude.Text ->
  -- | 'status'
  IntelligentTieringStatus ->
  IntelligentTieringConfiguration
mkIntelligentTieringConfiguration pId_ pStatus_ =
  IntelligentTieringConfiguration'
    { filter = Lude.Nothing,
      id = pId_,
      status = pStatus_,
      tierings = Lude.mempty
    }

-- | Specifies a bucket filter. The configuration only includes objects that meet the filter's criteria.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcFilter :: Lens.Lens' IntelligentTieringConfiguration (Lude.Maybe IntelligentTieringFilter)
itcFilter = Lens.lens (filter :: IntelligentTieringConfiguration -> Lude.Maybe IntelligentTieringFilter) (\s a -> s {filter = a} :: IntelligentTieringConfiguration)
{-# DEPRECATED itcFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The ID used to identify the S3 Intelligent-Tiering configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcId :: Lens.Lens' IntelligentTieringConfiguration Lude.Text
itcId = Lens.lens (id :: IntelligentTieringConfiguration -> Lude.Text) (\s a -> s {id = a} :: IntelligentTieringConfiguration)
{-# DEPRECATED itcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Specifies the status of the configuration.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcStatus :: Lens.Lens' IntelligentTieringConfiguration IntelligentTieringStatus
itcStatus = Lens.lens (status :: IntelligentTieringConfiguration -> IntelligentTieringStatus) (\s a -> s {status = a} :: IntelligentTieringConfiguration)
{-# DEPRECATED itcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies the S3 Intelligent-Tiering storage class tier of the configuration.
--
-- /Note:/ Consider using 'tierings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcTierings :: Lens.Lens' IntelligentTieringConfiguration [Tiering]
itcTierings = Lens.lens (tierings :: IntelligentTieringConfiguration -> [Tiering]) (\s a -> s {tierings = a} :: IntelligentTieringConfiguration)
{-# DEPRECATED itcTierings "Use generic-lens or generic-optics with 'tierings' instead." #-}

instance Lude.FromXML IntelligentTieringConfiguration where
  parseXML x =
    IntelligentTieringConfiguration'
      Lude.<$> (x Lude..@? "Filter")
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "Status")
      Lude.<*> (Lude.parseXMLList "Tiering" x)

instance Lude.ToXML IntelligentTieringConfiguration where
  toXML IntelligentTieringConfiguration' {..} =
    Lude.mconcat
      [ "Filter" Lude.@= filter,
        "Id" Lude.@= id,
        "Status" Lude.@= status,
        Lude.toXMLList "Tiering" tierings
      ]
