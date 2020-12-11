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
    worSubDomain,
    worProductListingIds,
    worNotificationConfiguration,
    worCreateDate,
    worWorkforceARN,
    worLastUpdatedDate,
    worWorkteamName,
    worMemberDefinitions,
    worWorkteamARN,
    worDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.MemberDefinition
import Network.AWS.SageMaker.Types.NotificationConfiguration

-- | Provides details about a labeling work team.
--
-- /See:/ 'mkWorkteam' smart constructor.
data Workteam = Workteam'
  { subDomain :: Lude.Maybe Lude.Text,
    productListingIds :: Lude.Maybe [Lude.Text],
    notificationConfiguration :: Lude.Maybe NotificationConfiguration,
    createDate :: Lude.Maybe Lude.Timestamp,
    workforceARN :: Lude.Maybe Lude.Text,
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp,
    workteamName :: Lude.Text,
    memberDefinitions :: Lude.NonEmpty MemberDefinition,
    workteamARN :: Lude.Text,
    description :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Workteam' with the minimum fields required to make a request.
--
-- * 'createDate' - The date and time that the work team was created (timestamp).
-- * 'description' - A description of the work team.
-- * 'lastUpdatedDate' - The date and time that the work team was last updated (timestamp).
-- * 'memberDefinitions' - A list of @MemberDefinition@ objects that contains objects that identify the workers that make up the work team.
--
-- Workforces can be created using Amazon Cognito or your own OIDC Identity Provider (IdP). For private workforces created using Amazon Cognito use @CognitoMemberDefinition@ . For workforces created using your own OIDC identity provider (IdP) use @OidcMemberDefinition@ .
-- * 'notificationConfiguration' - Configures SNS notifications of available or expiring work items for work teams.
-- * 'productListingIds' - The Amazon Marketplace identifier for a vendor's work team.
-- * 'subDomain' - The URI of the labeling job's user interface. Workers open this URI to start labeling your data objects.
-- * 'workforceARN' - The Amazon Resource Name (ARN) of the workforce.
-- * 'workteamARN' - The Amazon Resource Name (ARN) that identifies the work team.
-- * 'workteamName' - The name of the work team.
mkWorkteam ::
  -- | 'workteamName'
  Lude.Text ->
  -- | 'memberDefinitions'
  Lude.NonEmpty MemberDefinition ->
  -- | 'workteamARN'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  Workteam
mkWorkteam
  pWorkteamName_
  pMemberDefinitions_
  pWorkteamARN_
  pDescription_ =
    Workteam'
      { subDomain = Lude.Nothing,
        productListingIds = Lude.Nothing,
        notificationConfiguration = Lude.Nothing,
        createDate = Lude.Nothing,
        workforceARN = Lude.Nothing,
        lastUpdatedDate = Lude.Nothing,
        workteamName = pWorkteamName_,
        memberDefinitions = pMemberDefinitions_,
        workteamARN = pWorkteamARN_,
        description = pDescription_
      }

-- | The URI of the labeling job's user interface. Workers open this URI to start labeling your data objects.
--
-- /Note:/ Consider using 'subDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
worSubDomain :: Lens.Lens' Workteam (Lude.Maybe Lude.Text)
worSubDomain = Lens.lens (subDomain :: Workteam -> Lude.Maybe Lude.Text) (\s a -> s {subDomain = a} :: Workteam)
{-# DEPRECATED worSubDomain "Use generic-lens or generic-optics with 'subDomain' instead." #-}

-- | The Amazon Marketplace identifier for a vendor's work team.
--
-- /Note:/ Consider using 'productListingIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
worProductListingIds :: Lens.Lens' Workteam (Lude.Maybe [Lude.Text])
worProductListingIds = Lens.lens (productListingIds :: Workteam -> Lude.Maybe [Lude.Text]) (\s a -> s {productListingIds = a} :: Workteam)
{-# DEPRECATED worProductListingIds "Use generic-lens or generic-optics with 'productListingIds' instead." #-}

-- | Configures SNS notifications of available or expiring work items for work teams.
--
-- /Note:/ Consider using 'notificationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
worNotificationConfiguration :: Lens.Lens' Workteam (Lude.Maybe NotificationConfiguration)
worNotificationConfiguration = Lens.lens (notificationConfiguration :: Workteam -> Lude.Maybe NotificationConfiguration) (\s a -> s {notificationConfiguration = a} :: Workteam)
{-# DEPRECATED worNotificationConfiguration "Use generic-lens or generic-optics with 'notificationConfiguration' instead." #-}

-- | The date and time that the work team was created (timestamp).
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
worCreateDate :: Lens.Lens' Workteam (Lude.Maybe Lude.Timestamp)
worCreateDate = Lens.lens (createDate :: Workteam -> Lude.Maybe Lude.Timestamp) (\s a -> s {createDate = a} :: Workteam)
{-# DEPRECATED worCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The Amazon Resource Name (ARN) of the workforce.
--
-- /Note:/ Consider using 'workforceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
worWorkforceARN :: Lens.Lens' Workteam (Lude.Maybe Lude.Text)
worWorkforceARN = Lens.lens (workforceARN :: Workteam -> Lude.Maybe Lude.Text) (\s a -> s {workforceARN = a} :: Workteam)
{-# DEPRECATED worWorkforceARN "Use generic-lens or generic-optics with 'workforceARN' instead." #-}

-- | The date and time that the work team was last updated (timestamp).
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
worLastUpdatedDate :: Lens.Lens' Workteam (Lude.Maybe Lude.Timestamp)
worLastUpdatedDate = Lens.lens (lastUpdatedDate :: Workteam -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: Workteam)
{-# DEPRECATED worLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | The name of the work team.
--
-- /Note:/ Consider using 'workteamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
worWorkteamName :: Lens.Lens' Workteam Lude.Text
worWorkteamName = Lens.lens (workteamName :: Workteam -> Lude.Text) (\s a -> s {workteamName = a} :: Workteam)
{-# DEPRECATED worWorkteamName "Use generic-lens or generic-optics with 'workteamName' instead." #-}

-- | A list of @MemberDefinition@ objects that contains objects that identify the workers that make up the work team.
--
-- Workforces can be created using Amazon Cognito or your own OIDC Identity Provider (IdP). For private workforces created using Amazon Cognito use @CognitoMemberDefinition@ . For workforces created using your own OIDC identity provider (IdP) use @OidcMemberDefinition@ .
--
-- /Note:/ Consider using 'memberDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
worMemberDefinitions :: Lens.Lens' Workteam (Lude.NonEmpty MemberDefinition)
worMemberDefinitions = Lens.lens (memberDefinitions :: Workteam -> Lude.NonEmpty MemberDefinition) (\s a -> s {memberDefinitions = a} :: Workteam)
{-# DEPRECATED worMemberDefinitions "Use generic-lens or generic-optics with 'memberDefinitions' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the work team.
--
-- /Note:/ Consider using 'workteamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
worWorkteamARN :: Lens.Lens' Workteam Lude.Text
worWorkteamARN = Lens.lens (workteamARN :: Workteam -> Lude.Text) (\s a -> s {workteamARN = a} :: Workteam)
{-# DEPRECATED worWorkteamARN "Use generic-lens or generic-optics with 'workteamARN' instead." #-}

-- | A description of the work team.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
worDescription :: Lens.Lens' Workteam Lude.Text
worDescription = Lens.lens (description :: Workteam -> Lude.Text) (\s a -> s {description = a} :: Workteam)
{-# DEPRECATED worDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON Workteam where
  parseJSON =
    Lude.withObject
      "Workteam"
      ( \x ->
          Workteam'
            Lude.<$> (x Lude..:? "SubDomain")
            Lude.<*> (x Lude..:? "ProductListingIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NotificationConfiguration")
            Lude.<*> (x Lude..:? "CreateDate")
            Lude.<*> (x Lude..:? "WorkforceArn")
            Lude.<*> (x Lude..:? "LastUpdatedDate")
            Lude.<*> (x Lude..: "WorkteamName")
            Lude.<*> (x Lude..: "MemberDefinitions")
            Lude.<*> (x Lude..: "WorkteamArn")
            Lude.<*> (x Lude..: "Description")
      )
