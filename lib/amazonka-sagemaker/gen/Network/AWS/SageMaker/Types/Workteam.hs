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
    wfSubDomain,
    wfProductListingIds,
    wfNotificationConfiguration,
    wfCreateDate,
    wfMemberDefinitions,
    wfWorkforceARN,
    wfWorkteamARN,
    wfWorkteamName,
    wfLastUpdatedDate,
    wfDescription,
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
  { -- | The URI of the labeling job's user interface. Workers open this URI to start labeling your data objects.
    subDomain :: Lude.Maybe Lude.Text,
    -- | The Amazon Marketplace identifier for a vendor's work team.
    productListingIds :: Lude.Maybe [Lude.Text],
    -- | Configures SNS notifications of available or expiring work items for work teams.
    notificationConfiguration :: Lude.Maybe NotificationConfiguration,
    -- | The date and time that the work team was created (timestamp).
    createDate :: Lude.Maybe Lude.Timestamp,
    -- | A list of @MemberDefinition@ objects that contains objects that identify the workers that make up the work team.
    --
    -- Workforces can be created using Amazon Cognito or your own OIDC Identity Provider (IdP). For private workforces created using Amazon Cognito use @CognitoMemberDefinition@ . For workforces created using your own OIDC identity provider (IdP) use @OidcMemberDefinition@ .
    memberDefinitions :: Lude.NonEmpty MemberDefinition,
    -- | The Amazon Resource Name (ARN) of the workforce.
    workforceARN :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) that identifies the work team.
    workteamARN :: Lude.Text,
    -- | The name of the work team.
    workteamName :: Lude.Text,
    -- | The date and time that the work team was last updated (timestamp).
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp,
    -- | A description of the work team.
    description :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Workteam' with the minimum fields required to make a request.
--
-- * 'subDomain' - The URI of the labeling job's user interface. Workers open this URI to start labeling your data objects.
-- * 'productListingIds' - The Amazon Marketplace identifier for a vendor's work team.
-- * 'notificationConfiguration' - Configures SNS notifications of available or expiring work items for work teams.
-- * 'createDate' - The date and time that the work team was created (timestamp).
-- * 'memberDefinitions' - A list of @MemberDefinition@ objects that contains objects that identify the workers that make up the work team.
--
-- Workforces can be created using Amazon Cognito or your own OIDC Identity Provider (IdP). For private workforces created using Amazon Cognito use @CognitoMemberDefinition@ . For workforces created using your own OIDC identity provider (IdP) use @OidcMemberDefinition@ .
-- * 'workforceARN' - The Amazon Resource Name (ARN) of the workforce.
-- * 'workteamARN' - The Amazon Resource Name (ARN) that identifies the work team.
-- * 'workteamName' - The name of the work team.
-- * 'lastUpdatedDate' - The date and time that the work team was last updated (timestamp).
-- * 'description' - A description of the work team.
mkWorkteam ::
  -- | 'memberDefinitions'
  Lude.NonEmpty MemberDefinition ->
  -- | 'workteamARN'
  Lude.Text ->
  -- | 'workteamName'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  Workteam
mkWorkteam
  pMemberDefinitions_
  pWorkteamARN_
  pWorkteamName_
  pDescription_ =
    Workteam'
      { subDomain = Lude.Nothing,
        productListingIds = Lude.Nothing,
        notificationConfiguration = Lude.Nothing,
        createDate = Lude.Nothing,
        memberDefinitions = pMemberDefinitions_,
        workforceARN = Lude.Nothing,
        workteamARN = pWorkteamARN_,
        workteamName = pWorkteamName_,
        lastUpdatedDate = Lude.Nothing,
        description = pDescription_
      }

-- | The URI of the labeling job's user interface. Workers open this URI to start labeling your data objects.
--
-- /Note:/ Consider using 'subDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfSubDomain :: Lens.Lens' Workteam (Lude.Maybe Lude.Text)
wfSubDomain = Lens.lens (subDomain :: Workteam -> Lude.Maybe Lude.Text) (\s a -> s {subDomain = a} :: Workteam)
{-# DEPRECATED wfSubDomain "Use generic-lens or generic-optics with 'subDomain' instead." #-}

-- | The Amazon Marketplace identifier for a vendor's work team.
--
-- /Note:/ Consider using 'productListingIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfProductListingIds :: Lens.Lens' Workteam (Lude.Maybe [Lude.Text])
wfProductListingIds = Lens.lens (productListingIds :: Workteam -> Lude.Maybe [Lude.Text]) (\s a -> s {productListingIds = a} :: Workteam)
{-# DEPRECATED wfProductListingIds "Use generic-lens or generic-optics with 'productListingIds' instead." #-}

-- | Configures SNS notifications of available or expiring work items for work teams.
--
-- /Note:/ Consider using 'notificationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfNotificationConfiguration :: Lens.Lens' Workteam (Lude.Maybe NotificationConfiguration)
wfNotificationConfiguration = Lens.lens (notificationConfiguration :: Workteam -> Lude.Maybe NotificationConfiguration) (\s a -> s {notificationConfiguration = a} :: Workteam)
{-# DEPRECATED wfNotificationConfiguration "Use generic-lens or generic-optics with 'notificationConfiguration' instead." #-}

-- | The date and time that the work team was created (timestamp).
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfCreateDate :: Lens.Lens' Workteam (Lude.Maybe Lude.Timestamp)
wfCreateDate = Lens.lens (createDate :: Workteam -> Lude.Maybe Lude.Timestamp) (\s a -> s {createDate = a} :: Workteam)
{-# DEPRECATED wfCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | A list of @MemberDefinition@ objects that contains objects that identify the workers that make up the work team.
--
-- Workforces can be created using Amazon Cognito or your own OIDC Identity Provider (IdP). For private workforces created using Amazon Cognito use @CognitoMemberDefinition@ . For workforces created using your own OIDC identity provider (IdP) use @OidcMemberDefinition@ .
--
-- /Note:/ Consider using 'memberDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfMemberDefinitions :: Lens.Lens' Workteam (Lude.NonEmpty MemberDefinition)
wfMemberDefinitions = Lens.lens (memberDefinitions :: Workteam -> Lude.NonEmpty MemberDefinition) (\s a -> s {memberDefinitions = a} :: Workteam)
{-# DEPRECATED wfMemberDefinitions "Use generic-lens or generic-optics with 'memberDefinitions' instead." #-}

-- | The Amazon Resource Name (ARN) of the workforce.
--
-- /Note:/ Consider using 'workforceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfWorkforceARN :: Lens.Lens' Workteam (Lude.Maybe Lude.Text)
wfWorkforceARN = Lens.lens (workforceARN :: Workteam -> Lude.Maybe Lude.Text) (\s a -> s {workforceARN = a} :: Workteam)
{-# DEPRECATED wfWorkforceARN "Use generic-lens or generic-optics with 'workforceARN' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the work team.
--
-- /Note:/ Consider using 'workteamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfWorkteamARN :: Lens.Lens' Workteam Lude.Text
wfWorkteamARN = Lens.lens (workteamARN :: Workteam -> Lude.Text) (\s a -> s {workteamARN = a} :: Workteam)
{-# DEPRECATED wfWorkteamARN "Use generic-lens or generic-optics with 'workteamARN' instead." #-}

-- | The name of the work team.
--
-- /Note:/ Consider using 'workteamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfWorkteamName :: Lens.Lens' Workteam Lude.Text
wfWorkteamName = Lens.lens (workteamName :: Workteam -> Lude.Text) (\s a -> s {workteamName = a} :: Workteam)
{-# DEPRECATED wfWorkteamName "Use generic-lens or generic-optics with 'workteamName' instead." #-}

-- | The date and time that the work team was last updated (timestamp).
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfLastUpdatedDate :: Lens.Lens' Workteam (Lude.Maybe Lude.Timestamp)
wfLastUpdatedDate = Lens.lens (lastUpdatedDate :: Workteam -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: Workteam)
{-# DEPRECATED wfLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | A description of the work team.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfDescription :: Lens.Lens' Workteam Lude.Text
wfDescription = Lens.lens (description :: Workteam -> Lude.Text) (\s a -> s {description = a} :: Workteam)
{-# DEPRECATED wfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

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
            Lude.<*> (x Lude..: "MemberDefinitions")
            Lude.<*> (x Lude..:? "WorkforceArn")
            Lude.<*> (x Lude..: "WorkteamArn")
            Lude.<*> (x Lude..: "WorkteamName")
            Lude.<*> (x Lude..:? "LastUpdatedDate")
            Lude.<*> (x Lude..: "Description")
      )
