{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationConformancePack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationConformancePack
  ( OrganizationConformancePack (..),

    -- * Smart constructor
    mkOrganizationConformancePack,

    -- * Lenses
    ocpOrganizationConformancePackARN,
    ocpDeliveryS3Bucket,
    ocpOrganizationConformancePackName,
    ocpDeliveryS3KeyPrefix,
    ocpConformancePackInputParameters,
    ocpExcludedAccounts,
    ocpLastUpdateTime,
  )
where

import Network.AWS.Config.Types.ConformancePackInputParameter
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An organization conformance pack that has information about conformance packs that AWS Config creates in member accounts.
--
-- /See:/ 'mkOrganizationConformancePack' smart constructor.
data OrganizationConformancePack = OrganizationConformancePack'
  { -- | Amazon Resource Name (ARN) of organization conformance pack.
    organizationConformancePackARN :: Lude.Text,
    -- | Location of an Amazon S3 bucket where AWS Config can deliver evaluation results and conformance pack template that is used to create a pack.
    deliveryS3Bucket :: Lude.Maybe Lude.Text,
    -- | The name you assign to an organization conformance pack.
    organizationConformancePackName :: Lude.Text,
    -- | Any folder structure you want to add to an Amazon S3 bucket.
    deliveryS3KeyPrefix :: Lude.Maybe Lude.Text,
    -- | A list of @ConformancePackInputParameter@ objects.
    conformancePackInputParameters :: Lude.Maybe [ConformancePackInputParameter],
    -- | A comma-separated list of accounts excluded from organization conformance pack.
    excludedAccounts :: Lude.Maybe [Lude.Text],
    -- | Last time when organization conformation pack was updated.
    lastUpdateTime :: Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationConformancePack' with the minimum fields required to make a request.
--
-- * 'organizationConformancePackARN' - Amazon Resource Name (ARN) of organization conformance pack.
-- * 'deliveryS3Bucket' - Location of an Amazon S3 bucket where AWS Config can deliver evaluation results and conformance pack template that is used to create a pack.
-- * 'organizationConformancePackName' - The name you assign to an organization conformance pack.
-- * 'deliveryS3KeyPrefix' - Any folder structure you want to add to an Amazon S3 bucket.
-- * 'conformancePackInputParameters' - A list of @ConformancePackInputParameter@ objects.
-- * 'excludedAccounts' - A comma-separated list of accounts excluded from organization conformance pack.
-- * 'lastUpdateTime' - Last time when organization conformation pack was updated.
mkOrganizationConformancePack ::
  -- | 'organizationConformancePackARN'
  Lude.Text ->
  -- | 'organizationConformancePackName'
  Lude.Text ->
  -- | 'lastUpdateTime'
  Lude.Timestamp ->
  OrganizationConformancePack
mkOrganizationConformancePack
  pOrganizationConformancePackARN_
  pOrganizationConformancePackName_
  pLastUpdateTime_ =
    OrganizationConformancePack'
      { organizationConformancePackARN =
          pOrganizationConformancePackARN_,
        deliveryS3Bucket = Lude.Nothing,
        organizationConformancePackName =
          pOrganizationConformancePackName_,
        deliveryS3KeyPrefix = Lude.Nothing,
        conformancePackInputParameters = Lude.Nothing,
        excludedAccounts = Lude.Nothing,
        lastUpdateTime = pLastUpdateTime_
      }

-- | Amazon Resource Name (ARN) of organization conformance pack.
--
-- /Note:/ Consider using 'organizationConformancePackARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpOrganizationConformancePackARN :: Lens.Lens' OrganizationConformancePack Lude.Text
ocpOrganizationConformancePackARN = Lens.lens (organizationConformancePackARN :: OrganizationConformancePack -> Lude.Text) (\s a -> s {organizationConformancePackARN = a} :: OrganizationConformancePack)
{-# DEPRECATED ocpOrganizationConformancePackARN "Use generic-lens or generic-optics with 'organizationConformancePackARN' instead." #-}

-- | Location of an Amazon S3 bucket where AWS Config can deliver evaluation results and conformance pack template that is used to create a pack.
--
-- /Note:/ Consider using 'deliveryS3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpDeliveryS3Bucket :: Lens.Lens' OrganizationConformancePack (Lude.Maybe Lude.Text)
ocpDeliveryS3Bucket = Lens.lens (deliveryS3Bucket :: OrganizationConformancePack -> Lude.Maybe Lude.Text) (\s a -> s {deliveryS3Bucket = a} :: OrganizationConformancePack)
{-# DEPRECATED ocpDeliveryS3Bucket "Use generic-lens or generic-optics with 'deliveryS3Bucket' instead." #-}

-- | The name you assign to an organization conformance pack.
--
-- /Note:/ Consider using 'organizationConformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpOrganizationConformancePackName :: Lens.Lens' OrganizationConformancePack Lude.Text
ocpOrganizationConformancePackName = Lens.lens (organizationConformancePackName :: OrganizationConformancePack -> Lude.Text) (\s a -> s {organizationConformancePackName = a} :: OrganizationConformancePack)
{-# DEPRECATED ocpOrganizationConformancePackName "Use generic-lens or generic-optics with 'organizationConformancePackName' instead." #-}

-- | Any folder structure you want to add to an Amazon S3 bucket.
--
-- /Note:/ Consider using 'deliveryS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpDeliveryS3KeyPrefix :: Lens.Lens' OrganizationConformancePack (Lude.Maybe Lude.Text)
ocpDeliveryS3KeyPrefix = Lens.lens (deliveryS3KeyPrefix :: OrganizationConformancePack -> Lude.Maybe Lude.Text) (\s a -> s {deliveryS3KeyPrefix = a} :: OrganizationConformancePack)
{-# DEPRECATED ocpDeliveryS3KeyPrefix "Use generic-lens or generic-optics with 'deliveryS3KeyPrefix' instead." #-}

-- | A list of @ConformancePackInputParameter@ objects.
--
-- /Note:/ Consider using 'conformancePackInputParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpConformancePackInputParameters :: Lens.Lens' OrganizationConformancePack (Lude.Maybe [ConformancePackInputParameter])
ocpConformancePackInputParameters = Lens.lens (conformancePackInputParameters :: OrganizationConformancePack -> Lude.Maybe [ConformancePackInputParameter]) (\s a -> s {conformancePackInputParameters = a} :: OrganizationConformancePack)
{-# DEPRECATED ocpConformancePackInputParameters "Use generic-lens or generic-optics with 'conformancePackInputParameters' instead." #-}

-- | A comma-separated list of accounts excluded from organization conformance pack.
--
-- /Note:/ Consider using 'excludedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpExcludedAccounts :: Lens.Lens' OrganizationConformancePack (Lude.Maybe [Lude.Text])
ocpExcludedAccounts = Lens.lens (excludedAccounts :: OrganizationConformancePack -> Lude.Maybe [Lude.Text]) (\s a -> s {excludedAccounts = a} :: OrganizationConformancePack)
{-# DEPRECATED ocpExcludedAccounts "Use generic-lens or generic-optics with 'excludedAccounts' instead." #-}

-- | Last time when organization conformation pack was updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpLastUpdateTime :: Lens.Lens' OrganizationConformancePack Lude.Timestamp
ocpLastUpdateTime = Lens.lens (lastUpdateTime :: OrganizationConformancePack -> Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: OrganizationConformancePack)
{-# DEPRECATED ocpLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

instance Lude.FromJSON OrganizationConformancePack where
  parseJSON =
    Lude.withObject
      "OrganizationConformancePack"
      ( \x ->
          OrganizationConformancePack'
            Lude.<$> (x Lude..: "OrganizationConformancePackArn")
            Lude.<*> (x Lude..:? "DeliveryS3Bucket")
            Lude.<*> (x Lude..: "OrganizationConformancePackName")
            Lude.<*> (x Lude..:? "DeliveryS3KeyPrefix")
            Lude.<*> (x Lude..:? "ConformancePackInputParameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ExcludedAccounts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "LastUpdateTime")
      )
