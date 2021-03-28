{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Workforce
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.Workforce
  ( Workforce (..)
  -- * Smart constructor
  , mkWorkforce
  -- * Lenses
  , wWorkforceName
  , wWorkforceArn
  , wCognitoConfig
  , wCreateDate
  , wLastUpdatedDate
  , wOidcConfig
  , wSourceIpConfig
  , wSubDomain
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.CognitoConfig as Types
import qualified Network.AWS.SageMaker.Types.OidcConfigForResponse as Types
import qualified Network.AWS.SageMaker.Types.SourceIpConfig as Types
import qualified Network.AWS.SageMaker.Types.WorkforceArn as Types
import qualified Network.AWS.SageMaker.Types.WorkforceName as Types

-- | A single private workforce, which is automatically created when you create your first private work team. You can create one private work force in each AWS Region. By default, any workforce-related API operation used in a specific region will apply to the workforce created in that region. To learn how to create a private workforce, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce> .
--
-- /See:/ 'mkWorkforce' smart constructor.
data Workforce = Workforce'
  { workforceName :: Types.WorkforceName
    -- ^ The name of the private workforce.
  , workforceArn :: Types.WorkforceArn
    -- ^ The Amazon Resource Name (ARN) of the private workforce.
  , cognitoConfig :: Core.Maybe Types.CognitoConfig
    -- ^ The configuration of an Amazon Cognito workforce. A single Cognito workforce is created using and corresponds to a single <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool> .
  , createDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the workforce is created.
  , lastUpdatedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The most recent date that was used to successfully add one or more IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) to a private workforce's allow list.
  , oidcConfig :: Core.Maybe Types.OidcConfigForResponse
    -- ^ The configuration of an OIDC Identity Provider (IdP) private workforce.
  , sourceIpConfig :: Core.Maybe Types.SourceIpConfig
    -- ^ A list of one to ten IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) to be added to the workforce allow list. By default, a workforce isn't restricted to specific IP addresses.
  , subDomain :: Core.Maybe Core.Text
    -- ^ The subdomain for your OIDC Identity Provider.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Workforce' value with any optional fields omitted.
mkWorkforce
    :: Types.WorkforceName -- ^ 'workforceName'
    -> Types.WorkforceArn -- ^ 'workforceArn'
    -> Workforce
mkWorkforce workforceName workforceArn
  = Workforce'{workforceName, workforceArn,
               cognitoConfig = Core.Nothing, createDate = Core.Nothing,
               lastUpdatedDate = Core.Nothing, oidcConfig = Core.Nothing,
               sourceIpConfig = Core.Nothing, subDomain = Core.Nothing}

-- | The name of the private workforce.
--
-- /Note:/ Consider using 'workforceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wWorkforceName :: Lens.Lens' Workforce Types.WorkforceName
wWorkforceName = Lens.field @"workforceName"
{-# INLINEABLE wWorkforceName #-}
{-# DEPRECATED workforceName "Use generic-lens or generic-optics with 'workforceName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the private workforce.
--
-- /Note:/ Consider using 'workforceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wWorkforceArn :: Lens.Lens' Workforce Types.WorkforceArn
wWorkforceArn = Lens.field @"workforceArn"
{-# INLINEABLE wWorkforceArn #-}
{-# DEPRECATED workforceArn "Use generic-lens or generic-optics with 'workforceArn' instead"  #-}

-- | The configuration of an Amazon Cognito workforce. A single Cognito workforce is created using and corresponds to a single <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool> .
--
-- /Note:/ Consider using 'cognitoConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wCognitoConfig :: Lens.Lens' Workforce (Core.Maybe Types.CognitoConfig)
wCognitoConfig = Lens.field @"cognitoConfig"
{-# INLINEABLE wCognitoConfig #-}
{-# DEPRECATED cognitoConfig "Use generic-lens or generic-optics with 'cognitoConfig' instead"  #-}

-- | The date that the workforce is created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wCreateDate :: Lens.Lens' Workforce (Core.Maybe Core.NominalDiffTime)
wCreateDate = Lens.field @"createDate"
{-# INLINEABLE wCreateDate #-}
{-# DEPRECATED createDate "Use generic-lens or generic-optics with 'createDate' instead"  #-}

-- | The most recent date that was used to successfully add one or more IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) to a private workforce's allow list.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wLastUpdatedDate :: Lens.Lens' Workforce (Core.Maybe Core.NominalDiffTime)
wLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# INLINEABLE wLastUpdatedDate #-}
{-# DEPRECATED lastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead"  #-}

-- | The configuration of an OIDC Identity Provider (IdP) private workforce.
--
-- /Note:/ Consider using 'oidcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wOidcConfig :: Lens.Lens' Workforce (Core.Maybe Types.OidcConfigForResponse)
wOidcConfig = Lens.field @"oidcConfig"
{-# INLINEABLE wOidcConfig #-}
{-# DEPRECATED oidcConfig "Use generic-lens or generic-optics with 'oidcConfig' instead"  #-}

-- | A list of one to ten IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) to be added to the workforce allow list. By default, a workforce isn't restricted to specific IP addresses.
--
-- /Note:/ Consider using 'sourceIpConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wSourceIpConfig :: Lens.Lens' Workforce (Core.Maybe Types.SourceIpConfig)
wSourceIpConfig = Lens.field @"sourceIpConfig"
{-# INLINEABLE wSourceIpConfig #-}
{-# DEPRECATED sourceIpConfig "Use generic-lens or generic-optics with 'sourceIpConfig' instead"  #-}

-- | The subdomain for your OIDC Identity Provider.
--
-- /Note:/ Consider using 'subDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wSubDomain :: Lens.Lens' Workforce (Core.Maybe Core.Text)
wSubDomain = Lens.field @"subDomain"
{-# INLINEABLE wSubDomain #-}
{-# DEPRECATED subDomain "Use generic-lens or generic-optics with 'subDomain' instead"  #-}

instance Core.FromJSON Workforce where
        parseJSON
          = Core.withObject "Workforce" Core.$
              \ x ->
                Workforce' Core.<$>
                  (x Core..: "WorkforceName") Core.<*> x Core..: "WorkforceArn"
                    Core.<*> x Core..:? "CognitoConfig"
                    Core.<*> x Core..:? "CreateDate"
                    Core.<*> x Core..:? "LastUpdatedDate"
                    Core.<*> x Core..:? "OidcConfig"
                    Core.<*> x Core..:? "SourceIpConfig"
                    Core.<*> x Core..:? "SubDomain"
