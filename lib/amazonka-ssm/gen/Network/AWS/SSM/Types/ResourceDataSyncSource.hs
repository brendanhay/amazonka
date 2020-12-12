{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncSource
  ( ResourceDataSyncSource (..),

    -- * Smart constructor
    mkResourceDataSyncSource,

    -- * Lenses
    rdssIncludeFutureRegions,
    rdssAWSOrganizationsSource,
    rdssSourceType,
    rdssSourceRegions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.ResourceDataSyncAWSOrganizationsSource

-- | Information about the source of the data included in the resource data sync.
--
-- /See:/ 'mkResourceDataSyncSource' smart constructor.
data ResourceDataSyncSource = ResourceDataSyncSource'
  { includeFutureRegions ::
      Lude.Maybe Lude.Bool,
    awsOrganizationsSource ::
      Lude.Maybe
        ResourceDataSyncAWSOrganizationsSource,
    sourceType :: Lude.Text,
    sourceRegions :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceDataSyncSource' with the minimum fields required to make a request.
--
-- * 'awsOrganizationsSource' - Information about the AwsOrganizationsSource resource data sync source. A sync source of this type can synchronize data from AWS Organizations.
-- * 'includeFutureRegions' - Whether to automatically synchronize and aggregate data from new AWS Regions when those Regions come online.
-- * 'sourceRegions' - The @SyncSource@ AWS Regions included in the resource data sync.
-- * 'sourceType' - The type of data source for the resource data sync. @SourceType@ is either @AwsOrganizations@ (if an organization is present in AWS Organizations) or @singleAccountMultiRegions@ .
mkResourceDataSyncSource ::
  -- | 'sourceType'
  Lude.Text ->
  ResourceDataSyncSource
mkResourceDataSyncSource pSourceType_ =
  ResourceDataSyncSource'
    { includeFutureRegions = Lude.Nothing,
      awsOrganizationsSource = Lude.Nothing,
      sourceType = pSourceType_,
      sourceRegions = Lude.mempty
    }

-- | Whether to automatically synchronize and aggregate data from new AWS Regions when those Regions come online.
--
-- /Note:/ Consider using 'includeFutureRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssIncludeFutureRegions :: Lens.Lens' ResourceDataSyncSource (Lude.Maybe Lude.Bool)
rdssIncludeFutureRegions = Lens.lens (includeFutureRegions :: ResourceDataSyncSource -> Lude.Maybe Lude.Bool) (\s a -> s {includeFutureRegions = a} :: ResourceDataSyncSource)
{-# DEPRECATED rdssIncludeFutureRegions "Use generic-lens or generic-optics with 'includeFutureRegions' instead." #-}

-- | Information about the AwsOrganizationsSource resource data sync source. A sync source of this type can synchronize data from AWS Organizations.
--
-- /Note:/ Consider using 'awsOrganizationsSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssAWSOrganizationsSource :: Lens.Lens' ResourceDataSyncSource (Lude.Maybe ResourceDataSyncAWSOrganizationsSource)
rdssAWSOrganizationsSource = Lens.lens (awsOrganizationsSource :: ResourceDataSyncSource -> Lude.Maybe ResourceDataSyncAWSOrganizationsSource) (\s a -> s {awsOrganizationsSource = a} :: ResourceDataSyncSource)
{-# DEPRECATED rdssAWSOrganizationsSource "Use generic-lens or generic-optics with 'awsOrganizationsSource' instead." #-}

-- | The type of data source for the resource data sync. @SourceType@ is either @AwsOrganizations@ (if an organization is present in AWS Organizations) or @singleAccountMultiRegions@ .
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssSourceType :: Lens.Lens' ResourceDataSyncSource Lude.Text
rdssSourceType = Lens.lens (sourceType :: ResourceDataSyncSource -> Lude.Text) (\s a -> s {sourceType = a} :: ResourceDataSyncSource)
{-# DEPRECATED rdssSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The @SyncSource@ AWS Regions included in the resource data sync.
--
-- /Note:/ Consider using 'sourceRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssSourceRegions :: Lens.Lens' ResourceDataSyncSource [Lude.Text]
rdssSourceRegions = Lens.lens (sourceRegions :: ResourceDataSyncSource -> [Lude.Text]) (\s a -> s {sourceRegions = a} :: ResourceDataSyncSource)
{-# DEPRECATED rdssSourceRegions "Use generic-lens or generic-optics with 'sourceRegions' instead." #-}

instance Lude.ToJSON ResourceDataSyncSource where
  toJSON ResourceDataSyncSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IncludeFutureRegions" Lude..=) Lude.<$> includeFutureRegions,
            ("AwsOrganizationsSource" Lude..=) Lude.<$> awsOrganizationsSource,
            Lude.Just ("SourceType" Lude..= sourceType),
            Lude.Just ("SourceRegions" Lude..= sourceRegions)
          ]
      )
