{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Workforce
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Workforce
  ( Workforce (..),

    -- * Smart constructor
    mkWorkforce,

    -- * Lenses
    wSubDomain,
    wCreateDate,
    wSourceIPConfig,
    wWorkforceARN,
    wCognitoConfig,
    wLastUpdatedDate,
    wOidcConfig,
    wWorkforceName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.CognitoConfig
import Network.AWS.SageMaker.Types.OidcConfigForResponse
import Network.AWS.SageMaker.Types.SourceIPConfig

-- | A single private workforce, which is automatically created when you create your first private work team. You can create one private work force in each AWS Region. By default, any workforce-related API operation used in a specific region will apply to the workforce created in that region. To learn how to create a private workforce, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce> .
--
-- /See:/ 'mkWorkforce' smart constructor.
data Workforce = Workforce'
  { -- | The subdomain for your OIDC Identity Provider.
    subDomain :: Lude.Maybe Lude.Text,
    -- | The date that the workforce is created.
    createDate :: Lude.Maybe Lude.Timestamp,
    -- | A list of one to ten IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) to be added to the workforce allow list. By default, a workforce isn't restricted to specific IP addresses.
    sourceIPConfig :: Lude.Maybe SourceIPConfig,
    -- | The Amazon Resource Name (ARN) of the private workforce.
    workforceARN :: Lude.Text,
    -- | The configuration of an Amazon Cognito workforce. A single Cognito workforce is created using and corresponds to a single <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool> .
    cognitoConfig :: Lude.Maybe CognitoConfig,
    -- | The most recent date that was used to successfully add one or more IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) to a private workforce's allow list.
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp,
    -- | The configuration of an OIDC Identity Provider (IdP) private workforce.
    oidcConfig :: Lude.Maybe OidcConfigForResponse,
    -- | The name of the private workforce.
    workforceName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Workforce' with the minimum fields required to make a request.
--
-- * 'subDomain' - The subdomain for your OIDC Identity Provider.
-- * 'createDate' - The date that the workforce is created.
-- * 'sourceIPConfig' - A list of one to ten IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) to be added to the workforce allow list. By default, a workforce isn't restricted to specific IP addresses.
-- * 'workforceARN' - The Amazon Resource Name (ARN) of the private workforce.
-- * 'cognitoConfig' - The configuration of an Amazon Cognito workforce. A single Cognito workforce is created using and corresponds to a single <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool> .
-- * 'lastUpdatedDate' - The most recent date that was used to successfully add one or more IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) to a private workforce's allow list.
-- * 'oidcConfig' - The configuration of an OIDC Identity Provider (IdP) private workforce.
-- * 'workforceName' - The name of the private workforce.
mkWorkforce ::
  -- | 'workforceARN'
  Lude.Text ->
  -- | 'workforceName'
  Lude.Text ->
  Workforce
mkWorkforce pWorkforceARN_ pWorkforceName_ =
  Workforce'
    { subDomain = Lude.Nothing,
      createDate = Lude.Nothing,
      sourceIPConfig = Lude.Nothing,
      workforceARN = pWorkforceARN_,
      cognitoConfig = Lude.Nothing,
      lastUpdatedDate = Lude.Nothing,
      oidcConfig = Lude.Nothing,
      workforceName = pWorkforceName_
    }

-- | The subdomain for your OIDC Identity Provider.
--
-- /Note:/ Consider using 'subDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wSubDomain :: Lens.Lens' Workforce (Lude.Maybe Lude.Text)
wSubDomain = Lens.lens (subDomain :: Workforce -> Lude.Maybe Lude.Text) (\s a -> s {subDomain = a} :: Workforce)
{-# DEPRECATED wSubDomain "Use generic-lens or generic-optics with 'subDomain' instead." #-}

-- | The date that the workforce is created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wCreateDate :: Lens.Lens' Workforce (Lude.Maybe Lude.Timestamp)
wCreateDate = Lens.lens (createDate :: Workforce -> Lude.Maybe Lude.Timestamp) (\s a -> s {createDate = a} :: Workforce)
{-# DEPRECATED wCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | A list of one to ten IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) to be added to the workforce allow list. By default, a workforce isn't restricted to specific IP addresses.
--
-- /Note:/ Consider using 'sourceIPConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wSourceIPConfig :: Lens.Lens' Workforce (Lude.Maybe SourceIPConfig)
wSourceIPConfig = Lens.lens (sourceIPConfig :: Workforce -> Lude.Maybe SourceIPConfig) (\s a -> s {sourceIPConfig = a} :: Workforce)
{-# DEPRECATED wSourceIPConfig "Use generic-lens or generic-optics with 'sourceIPConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the private workforce.
--
-- /Note:/ Consider using 'workforceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wWorkforceARN :: Lens.Lens' Workforce Lude.Text
wWorkforceARN = Lens.lens (workforceARN :: Workforce -> Lude.Text) (\s a -> s {workforceARN = a} :: Workforce)
{-# DEPRECATED wWorkforceARN "Use generic-lens or generic-optics with 'workforceARN' instead." #-}

-- | The configuration of an Amazon Cognito workforce. A single Cognito workforce is created using and corresponds to a single <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool> .
--
-- /Note:/ Consider using 'cognitoConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wCognitoConfig :: Lens.Lens' Workforce (Lude.Maybe CognitoConfig)
wCognitoConfig = Lens.lens (cognitoConfig :: Workforce -> Lude.Maybe CognitoConfig) (\s a -> s {cognitoConfig = a} :: Workforce)
{-# DEPRECATED wCognitoConfig "Use generic-lens or generic-optics with 'cognitoConfig' instead." #-}

-- | The most recent date that was used to successfully add one or more IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) to a private workforce's allow list.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wLastUpdatedDate :: Lens.Lens' Workforce (Lude.Maybe Lude.Timestamp)
wLastUpdatedDate = Lens.lens (lastUpdatedDate :: Workforce -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: Workforce)
{-# DEPRECATED wLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | The configuration of an OIDC Identity Provider (IdP) private workforce.
--
-- /Note:/ Consider using 'oidcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wOidcConfig :: Lens.Lens' Workforce (Lude.Maybe OidcConfigForResponse)
wOidcConfig = Lens.lens (oidcConfig :: Workforce -> Lude.Maybe OidcConfigForResponse) (\s a -> s {oidcConfig = a} :: Workforce)
{-# DEPRECATED wOidcConfig "Use generic-lens or generic-optics with 'oidcConfig' instead." #-}

-- | The name of the private workforce.
--
-- /Note:/ Consider using 'workforceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wWorkforceName :: Lens.Lens' Workforce Lude.Text
wWorkforceName = Lens.lens (workforceName :: Workforce -> Lude.Text) (\s a -> s {workforceName = a} :: Workforce)
{-# DEPRECATED wWorkforceName "Use generic-lens or generic-optics with 'workforceName' instead." #-}

instance Lude.FromJSON Workforce where
  parseJSON =
    Lude.withObject
      "Workforce"
      ( \x ->
          Workforce'
            Lude.<$> (x Lude..:? "SubDomain")
            Lude.<*> (x Lude..:? "CreateDate")
            Lude.<*> (x Lude..:? "SourceIpConfig")
            Lude.<*> (x Lude..: "WorkforceArn")
            Lude.<*> (x Lude..:? "CognitoConfig")
            Lude.<*> (x Lude..:? "LastUpdatedDate")
            Lude.<*> (x Lude..:? "OidcConfig")
            Lude.<*> (x Lude..: "WorkforceName")
      )
