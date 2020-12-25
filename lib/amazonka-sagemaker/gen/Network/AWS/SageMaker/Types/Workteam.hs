{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Workteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Workteam
  ( Workteam (..),

    -- * Smart constructor
    mkWorkteam,

    -- * Lenses
    wfWorkteamName,
    wfMemberDefinitions,
    wfWorkteamArn,
    wfDescription,
    wfCreateDate,
    wfLastUpdatedDate,
    wfNotificationConfiguration,
    wfProductListingIds,
    wfSubDomain,
    wfWorkforceArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.Description as Types
import qualified Network.AWS.SageMaker.Types.MemberDefinition as Types
import qualified Network.AWS.SageMaker.Types.NotificationConfiguration as Types
import qualified Network.AWS.SageMaker.Types.String as Types
import qualified Network.AWS.SageMaker.Types.SubDomain as Types
import qualified Network.AWS.SageMaker.Types.WorkforceArn as Types
import qualified Network.AWS.SageMaker.Types.WorkteamArn as Types
import qualified Network.AWS.SageMaker.Types.WorkteamName as Types

-- | Provides details about a labeling work team.
--
-- /See:/ 'mkWorkteam' smart constructor.
data Workteam = Workteam'
  { -- | The name of the work team.
    workteamName :: Types.WorkteamName,
    -- | A list of @MemberDefinition@ objects that contains objects that identify the workers that make up the work team.
    --
    -- Workforces can be created using Amazon Cognito or your own OIDC Identity Provider (IdP). For private workforces created using Amazon Cognito use @CognitoMemberDefinition@ . For workforces created using your own OIDC identity provider (IdP) use @OidcMemberDefinition@ .
    memberDefinitions :: Core.NonEmpty Types.MemberDefinition,
    -- | The Amazon Resource Name (ARN) that identifies the work team.
    workteamArn :: Types.WorkteamArn,
    -- | A description of the work team.
    description :: Types.Description,
    -- | The date and time that the work team was created (timestamp).
    createDate :: Core.Maybe Core.NominalDiffTime,
    -- | The date and time that the work team was last updated (timestamp).
    lastUpdatedDate :: Core.Maybe Core.NominalDiffTime,
    -- | Configures SNS notifications of available or expiring work items for work teams.
    notificationConfiguration :: Core.Maybe Types.NotificationConfiguration,
    -- | The Amazon Marketplace identifier for a vendor's work team.
    productListingIds :: Core.Maybe [Types.String],
    -- | The URI of the labeling job's user interface. Workers open this URI to start labeling your data objects.
    subDomain :: Core.Maybe Types.SubDomain,
    -- | The Amazon Resource Name (ARN) of the workforce.
    workforceArn :: Core.Maybe Types.WorkforceArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Workteam' value with any optional fields omitted.
mkWorkteam ::
  -- | 'workteamName'
  Types.WorkteamName ->
  -- | 'memberDefinitions'
  Core.NonEmpty Types.MemberDefinition ->
  -- | 'workteamArn'
  Types.WorkteamArn ->
  -- | 'description'
  Types.Description ->
  Workteam
mkWorkteam workteamName memberDefinitions workteamArn description =
  Workteam'
    { workteamName,
      memberDefinitions,
      workteamArn,
      description,
      createDate = Core.Nothing,
      lastUpdatedDate = Core.Nothing,
      notificationConfiguration = Core.Nothing,
      productListingIds = Core.Nothing,
      subDomain = Core.Nothing,
      workforceArn = Core.Nothing
    }

-- | The name of the work team.
--
-- /Note:/ Consider using 'workteamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfWorkteamName :: Lens.Lens' Workteam Types.WorkteamName
wfWorkteamName = Lens.field @"workteamName"
{-# DEPRECATED wfWorkteamName "Use generic-lens or generic-optics with 'workteamName' instead." #-}

-- | A list of @MemberDefinition@ objects that contains objects that identify the workers that make up the work team.
--
-- Workforces can be created using Amazon Cognito or your own OIDC Identity Provider (IdP). For private workforces created using Amazon Cognito use @CognitoMemberDefinition@ . For workforces created using your own OIDC identity provider (IdP) use @OidcMemberDefinition@ .
--
-- /Note:/ Consider using 'memberDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfMemberDefinitions :: Lens.Lens' Workteam (Core.NonEmpty Types.MemberDefinition)
wfMemberDefinitions = Lens.field @"memberDefinitions"
{-# DEPRECATED wfMemberDefinitions "Use generic-lens or generic-optics with 'memberDefinitions' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the work team.
--
-- /Note:/ Consider using 'workteamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfWorkteamArn :: Lens.Lens' Workteam Types.WorkteamArn
wfWorkteamArn = Lens.field @"workteamArn"
{-# DEPRECATED wfWorkteamArn "Use generic-lens or generic-optics with 'workteamArn' instead." #-}

-- | A description of the work team.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfDescription :: Lens.Lens' Workteam Types.Description
wfDescription = Lens.field @"description"
{-# DEPRECATED wfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The date and time that the work team was created (timestamp).
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfCreateDate :: Lens.Lens' Workteam (Core.Maybe Core.NominalDiffTime)
wfCreateDate = Lens.field @"createDate"
{-# DEPRECATED wfCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The date and time that the work team was last updated (timestamp).
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfLastUpdatedDate :: Lens.Lens' Workteam (Core.Maybe Core.NominalDiffTime)
wfLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# DEPRECATED wfLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | Configures SNS notifications of available or expiring work items for work teams.
--
-- /Note:/ Consider using 'notificationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfNotificationConfiguration :: Lens.Lens' Workteam (Core.Maybe Types.NotificationConfiguration)
wfNotificationConfiguration = Lens.field @"notificationConfiguration"
{-# DEPRECATED wfNotificationConfiguration "Use generic-lens or generic-optics with 'notificationConfiguration' instead." #-}

-- | The Amazon Marketplace identifier for a vendor's work team.
--
-- /Note:/ Consider using 'productListingIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfProductListingIds :: Lens.Lens' Workteam (Core.Maybe [Types.String])
wfProductListingIds = Lens.field @"productListingIds"
{-# DEPRECATED wfProductListingIds "Use generic-lens or generic-optics with 'productListingIds' instead." #-}

-- | The URI of the labeling job's user interface. Workers open this URI to start labeling your data objects.
--
-- /Note:/ Consider using 'subDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfSubDomain :: Lens.Lens' Workteam (Core.Maybe Types.SubDomain)
wfSubDomain = Lens.field @"subDomain"
{-# DEPRECATED wfSubDomain "Use generic-lens or generic-optics with 'subDomain' instead." #-}

-- | The Amazon Resource Name (ARN) of the workforce.
--
-- /Note:/ Consider using 'workforceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfWorkforceArn :: Lens.Lens' Workteam (Core.Maybe Types.WorkforceArn)
wfWorkforceArn = Lens.field @"workforceArn"
{-# DEPRECATED wfWorkforceArn "Use generic-lens or generic-optics with 'workforceArn' instead." #-}

instance Core.FromJSON Workteam where
  parseJSON =
    Core.withObject "Workteam" Core.$
      \x ->
        Workteam'
          Core.<$> (x Core..: "WorkteamName")
          Core.<*> (x Core..: "MemberDefinitions")
          Core.<*> (x Core..: "WorkteamArn")
          Core.<*> (x Core..: "Description")
          Core.<*> (x Core..:? "CreateDate")
          Core.<*> (x Core..:? "LastUpdatedDate")
          Core.<*> (x Core..:? "NotificationConfiguration")
          Core.<*> (x Core..:? "ProductListingIds")
          Core.<*> (x Core..:? "SubDomain")
          Core.<*> (x Core..:? "WorkforceArn")
