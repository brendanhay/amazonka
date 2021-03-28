{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateWorkforce
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to create a workforce. This operation will return an error if a workforce already exists in the AWS Region that you specify. You can only create one workforce in each AWS Region per AWS account.
--
-- If you want to create a new workforce in an AWS Region where a workforce already exists, use the API operation to delete the existing workforce and then use @CreateWorkforce@ to create a new workforce.
-- To create a private workforce using Amazon Cognito, you must specify a Cognito user pool in @CognitoConfig@ . You can also create an Amazon Cognito workforce using the Amazon SageMaker console. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce (Amazon Cognito)> .
-- To create a private workforce using your own OIDC Identity Provider (IdP), specify your IdP configuration in @OidcConfig@ . Your OIDC IdP must support /groups/ because groups are used by Ground Truth and Amazon A2I to create work teams. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private-oidc.html Create a Private Workforce (OIDC IdP)> .
module Network.AWS.SageMaker.CreateWorkforce
    (
    -- * Creating a request
      CreateWorkforce (..)
    , mkCreateWorkforce
    -- ** Request lenses
    , cwfWorkforceName
    , cwfCognitoConfig
    , cwfOidcConfig
    , cwfSourceIpConfig
    , cwfTags

    -- * Destructuring the response
    , CreateWorkforceResponse (..)
    , mkCreateWorkforceResponse
    -- ** Response lenses
    , cwrrsWorkforceArn
    , cwrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateWorkforce' smart constructor.
data CreateWorkforce = CreateWorkforce'
  { workforceName :: Types.WorkforceName
    -- ^ The name of the private workforce.
  , cognitoConfig :: Core.Maybe Types.CognitoConfig
    -- ^ Use this parameter to configure an Amazon Cognito private workforce. A single Cognito workforce is created using and corresponds to a single <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool> .
--
-- Do not use @OidcConfig@ if you specify values for @CognitoConfig@ .
  , oidcConfig :: Core.Maybe Types.OidcConfig
    -- ^ Use this parameter to configure a private workforce using your own OIDC Identity Provider.
--
-- Do not use @CognitoConfig@ if you specify values for @OidcConfig@ .
  , sourceIpConfig :: Core.Maybe Types.SourceIpConfig
  , tags :: Core.Maybe [Types.Tag]
    -- ^ An array of key-value pairs that contain metadata to help you categorize and organize our workforce. Each tag consists of a key and a value, both of which you define.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateWorkforce' value with any optional fields omitted.
mkCreateWorkforce
    :: Types.WorkforceName -- ^ 'workforceName'
    -> CreateWorkforce
mkCreateWorkforce workforceName
  = CreateWorkforce'{workforceName, cognitoConfig = Core.Nothing,
                     oidcConfig = Core.Nothing, sourceIpConfig = Core.Nothing,
                     tags = Core.Nothing}

-- | The name of the private workforce.
--
-- /Note:/ Consider using 'workforceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwfWorkforceName :: Lens.Lens' CreateWorkforce Types.WorkforceName
cwfWorkforceName = Lens.field @"workforceName"
{-# INLINEABLE cwfWorkforceName #-}
{-# DEPRECATED workforceName "Use generic-lens or generic-optics with 'workforceName' instead"  #-}

-- | Use this parameter to configure an Amazon Cognito private workforce. A single Cognito workforce is created using and corresponds to a single <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool> .
--
-- Do not use @OidcConfig@ if you specify values for @CognitoConfig@ .
--
-- /Note:/ Consider using 'cognitoConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwfCognitoConfig :: Lens.Lens' CreateWorkforce (Core.Maybe Types.CognitoConfig)
cwfCognitoConfig = Lens.field @"cognitoConfig"
{-# INLINEABLE cwfCognitoConfig #-}
{-# DEPRECATED cognitoConfig "Use generic-lens or generic-optics with 'cognitoConfig' instead"  #-}

-- | Use this parameter to configure a private workforce using your own OIDC Identity Provider.
--
-- Do not use @CognitoConfig@ if you specify values for @OidcConfig@ .
--
-- /Note:/ Consider using 'oidcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwfOidcConfig :: Lens.Lens' CreateWorkforce (Core.Maybe Types.OidcConfig)
cwfOidcConfig = Lens.field @"oidcConfig"
{-# INLINEABLE cwfOidcConfig #-}
{-# DEPRECATED oidcConfig "Use generic-lens or generic-optics with 'oidcConfig' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sourceIpConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwfSourceIpConfig :: Lens.Lens' CreateWorkforce (Core.Maybe Types.SourceIpConfig)
cwfSourceIpConfig = Lens.field @"sourceIpConfig"
{-# INLINEABLE cwfSourceIpConfig #-}
{-# DEPRECATED sourceIpConfig "Use generic-lens or generic-optics with 'sourceIpConfig' instead"  #-}

-- | An array of key-value pairs that contain metadata to help you categorize and organize our workforce. Each tag consists of a key and a value, both of which you define.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwfTags :: Lens.Lens' CreateWorkforce (Core.Maybe [Types.Tag])
cwfTags = Lens.field @"tags"
{-# INLINEABLE cwfTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateWorkforce where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateWorkforce where
        toHeaders CreateWorkforce{..}
          = Core.pure ("X-Amz-Target", "SageMaker.CreateWorkforce") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateWorkforce where
        toJSON CreateWorkforce{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WorkforceName" Core..= workforceName),
                  ("CognitoConfig" Core..=) Core.<$> cognitoConfig,
                  ("OidcConfig" Core..=) Core.<$> oidcConfig,
                  ("SourceIpConfig" Core..=) Core.<$> sourceIpConfig,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateWorkforce where
        type Rs CreateWorkforce = CreateWorkforceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateWorkforceResponse' Core.<$>
                   (x Core..: "WorkforceArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateWorkforceResponse' smart constructor.
data CreateWorkforceResponse = CreateWorkforceResponse'
  { workforceArn :: Types.WorkforceArn
    -- ^ The Amazon Resource Name (ARN) of the workforce.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateWorkforceResponse' value with any optional fields omitted.
mkCreateWorkforceResponse
    :: Types.WorkforceArn -- ^ 'workforceArn'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateWorkforceResponse
mkCreateWorkforceResponse workforceArn responseStatus
  = CreateWorkforceResponse'{workforceArn, responseStatus}

-- | The Amazon Resource Name (ARN) of the workforce.
--
-- /Note:/ Consider using 'workforceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwrrsWorkforceArn :: Lens.Lens' CreateWorkforceResponse Types.WorkforceArn
cwrrsWorkforceArn = Lens.field @"workforceArn"
{-# INLINEABLE cwrrsWorkforceArn #-}
{-# DEPRECATED workforceArn "Use generic-lens or generic-optics with 'workforceArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwrrsResponseStatus :: Lens.Lens' CreateWorkforceResponse Core.Int
cwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
