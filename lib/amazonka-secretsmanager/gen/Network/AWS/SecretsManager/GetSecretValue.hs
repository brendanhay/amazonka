{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.GetSecretValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the contents of the encrypted fields @SecretString@ or @SecretBinary@ from the specified version of a secret, whichever contains content.
--
-- __Minimum permissions__ 
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:GetSecretValue
--
--
--     * kms:Decrypt - required only if you use a customer-managed AWS KMS key to encrypt the secret. You do not need this permission to use the account's default AWS managed CMK for Secrets Manager.
--
--
-- __Related operations__ 
--
--     * To create a new version of the secret with different encrypted information, use 'PutSecretValue' .
--
--
--     * To retrieve the non-encrypted details for the secret, use 'DescribeSecret' .
--
--
module Network.AWS.SecretsManager.GetSecretValue
    (
    -- * Creating a request
      GetSecretValue (..)
    , mkGetSecretValue
    -- ** Request lenses
    , gsvSecretId
    , gsvVersionId
    , gsvVersionStage

    -- * Destructuring the response
    , GetSecretValueResponse (..)
    , mkGetSecretValueResponse
    -- ** Response lenses
    , gsvrrsARN
    , gsvrrsCreatedDate
    , gsvrrsName
    , gsvrrsSecretBinary
    , gsvrrsSecretString
    , gsvrrsVersionId
    , gsvrrsVersionStages
    , gsvrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SecretsManager.Types as Types

-- | /See:/ 'mkGetSecretValue' smart constructor.
data GetSecretValue = GetSecretValue'
  { secretId :: Types.SecretId
    -- ^ Specifies the secret containing the version that you want to retrieve. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
  , versionId :: Core.Maybe Types.VersionId
    -- ^ Specifies the unique identifier of the version of the secret that you want to retrieve. If you specify this parameter then don't specify @VersionStage@ . If you don't specify either a @VersionStage@ or @VersionId@ then the default is to perform the operation on the version with the @VersionStage@ value of @AWSCURRENT@ .
--
-- This value is typically a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value with 32 hexadecimal digits.
  , versionStage :: Core.Maybe Types.VersionStage
    -- ^ Specifies the secret version that you want to retrieve by the staging label attached to the version.
--
-- Staging labels are used to keep track of different versions during the rotation process. If you use this parameter then don't specify @VersionId@ . If you don't specify either a @VersionStage@ or @VersionId@ , then the default is to perform the operation on the version with the @VersionStage@ value of @AWSCURRENT@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSecretValue' value with any optional fields omitted.
mkGetSecretValue
    :: Types.SecretId -- ^ 'secretId'
    -> GetSecretValue
mkGetSecretValue secretId
  = GetSecretValue'{secretId, versionId = Core.Nothing,
                    versionStage = Core.Nothing}

-- | Specifies the secret containing the version that you want to retrieve. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvSecretId :: Lens.Lens' GetSecretValue Types.SecretId
gsvSecretId = Lens.field @"secretId"
{-# INLINEABLE gsvSecretId #-}
{-# DEPRECATED secretId "Use generic-lens or generic-optics with 'secretId' instead"  #-}

-- | Specifies the unique identifier of the version of the secret that you want to retrieve. If you specify this parameter then don't specify @VersionStage@ . If you don't specify either a @VersionStage@ or @VersionId@ then the default is to perform the operation on the version with the @VersionStage@ value of @AWSCURRENT@ .
--
-- This value is typically a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value with 32 hexadecimal digits.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvVersionId :: Lens.Lens' GetSecretValue (Core.Maybe Types.VersionId)
gsvVersionId = Lens.field @"versionId"
{-# INLINEABLE gsvVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

-- | Specifies the secret version that you want to retrieve by the staging label attached to the version.
--
-- Staging labels are used to keep track of different versions during the rotation process. If you use this parameter then don't specify @VersionId@ . If you don't specify either a @VersionStage@ or @VersionId@ , then the default is to perform the operation on the version with the @VersionStage@ value of @AWSCURRENT@ .
--
-- /Note:/ Consider using 'versionStage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvVersionStage :: Lens.Lens' GetSecretValue (Core.Maybe Types.VersionStage)
gsvVersionStage = Lens.field @"versionStage"
{-# INLINEABLE gsvVersionStage #-}
{-# DEPRECATED versionStage "Use generic-lens or generic-optics with 'versionStage' instead"  #-}

instance Core.ToQuery GetSecretValue where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSecretValue where
        toHeaders GetSecretValue{..}
          = Core.pure ("X-Amz-Target", "secretsmanager.GetSecretValue")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetSecretValue where
        toJSON GetSecretValue{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SecretId" Core..= secretId),
                  ("VersionId" Core..=) Core.<$> versionId,
                  ("VersionStage" Core..=) Core.<$> versionStage])

instance Core.AWSRequest GetSecretValue where
        type Rs GetSecretValue = GetSecretValueResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSecretValueResponse' Core.<$>
                   (x Core..:? "ARN") Core.<*> x Core..:? "CreatedDate" Core.<*>
                     x Core..:? "Name"
                     Core.<*> x Core..:? "SecretBinary"
                     Core.<*> x Core..:? "SecretString"
                     Core.<*> x Core..:? "VersionId"
                     Core.<*> x Core..:? "VersionStages"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetSecretValueResponse' smart constructor.
data GetSecretValueResponse = GetSecretValueResponse'
  { arn :: Core.Maybe Types.ARN
    -- ^ The ARN of the secret.
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that this version of the secret was created.
  , name :: Core.Maybe Types.Name
    -- ^ The friendly name of the secret.
  , secretBinary :: Core.Maybe (Core.Sensitive Core.Base64)
    -- ^ The decrypted part of the protected secret information that was originally provided as binary data in the form of a byte array. The response parameter represents the binary data as a <https://tools.ietf.org/html/rfc4648#section-4 base64-encoded> string.
--
-- This parameter is not used if the secret is created by the Secrets Manager console.
-- If you store custom information in this field of the secret, then you must code your Lambda rotation function to parse and interpret whatever you store in the @SecretString@ or @SecretBinary@ fields.
  , secretString :: Core.Maybe Types.SecretString
    -- ^ The decrypted part of the protected secret information that was originally provided as a string.
--
-- If you create this secret by using the Secrets Manager console then only the @SecretString@ parameter contains data. Secrets Manager stores the information as a JSON structure of key/value pairs that the Lambda rotation function knows how to parse.
-- If you store custom information in the secret by using the 'CreateSecret' , 'UpdateSecret' , or 'PutSecretValue' API operations instead of the Secrets Manager console, or by using the __Other secret type__ in the console, then you must code your Lambda rotation function to parse and interpret those values.
  , versionId :: Core.Maybe Types.VersionId
    -- ^ The unique identifier of this version of the secret.
  , versionStages :: Core.Maybe (Core.NonEmpty Types.SecretVersionStageType)
    -- ^ A list of all of the staging labels currently attached to this version of the secret.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetSecretValueResponse' value with any optional fields omitted.
mkGetSecretValueResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSecretValueResponse
mkGetSecretValueResponse responseStatus
  = GetSecretValueResponse'{arn = Core.Nothing,
                            createdDate = Core.Nothing, name = Core.Nothing,
                            secretBinary = Core.Nothing, secretString = Core.Nothing,
                            versionId = Core.Nothing, versionStages = Core.Nothing,
                            responseStatus}

-- | The ARN of the secret.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsARN :: Lens.Lens' GetSecretValueResponse (Core.Maybe Types.ARN)
gsvrrsARN = Lens.field @"arn"
{-# INLINEABLE gsvrrsARN #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The date and time that this version of the secret was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsCreatedDate :: Lens.Lens' GetSecretValueResponse (Core.Maybe Core.NominalDiffTime)
gsvrrsCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE gsvrrsCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | The friendly name of the secret.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsName :: Lens.Lens' GetSecretValueResponse (Core.Maybe Types.Name)
gsvrrsName = Lens.field @"name"
{-# INLINEABLE gsvrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The decrypted part of the protected secret information that was originally provided as binary data in the form of a byte array. The response parameter represents the binary data as a <https://tools.ietf.org/html/rfc4648#section-4 base64-encoded> string.
--
-- This parameter is not used if the secret is created by the Secrets Manager console.
-- If you store custom information in this field of the secret, then you must code your Lambda rotation function to parse and interpret whatever you store in the @SecretString@ or @SecretBinary@ fields.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'secretBinary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsSecretBinary :: Lens.Lens' GetSecretValueResponse (Core.Maybe (Core.Sensitive Core.Base64))
gsvrrsSecretBinary = Lens.field @"secretBinary"
{-# INLINEABLE gsvrrsSecretBinary #-}
{-# DEPRECATED secretBinary "Use generic-lens or generic-optics with 'secretBinary' instead"  #-}

-- | The decrypted part of the protected secret information that was originally provided as a string.
--
-- If you create this secret by using the Secrets Manager console then only the @SecretString@ parameter contains data. Secrets Manager stores the information as a JSON structure of key/value pairs that the Lambda rotation function knows how to parse.
-- If you store custom information in the secret by using the 'CreateSecret' , 'UpdateSecret' , or 'PutSecretValue' API operations instead of the Secrets Manager console, or by using the __Other secret type__ in the console, then you must code your Lambda rotation function to parse and interpret those values.
--
-- /Note:/ Consider using 'secretString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsSecretString :: Lens.Lens' GetSecretValueResponse (Core.Maybe Types.SecretString)
gsvrrsSecretString = Lens.field @"secretString"
{-# INLINEABLE gsvrrsSecretString #-}
{-# DEPRECATED secretString "Use generic-lens or generic-optics with 'secretString' instead"  #-}

-- | The unique identifier of this version of the secret.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsVersionId :: Lens.Lens' GetSecretValueResponse (Core.Maybe Types.VersionId)
gsvrrsVersionId = Lens.field @"versionId"
{-# INLINEABLE gsvrrsVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

-- | A list of all of the staging labels currently attached to this version of the secret.
--
-- /Note:/ Consider using 'versionStages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsVersionStages :: Lens.Lens' GetSecretValueResponse (Core.Maybe (Core.NonEmpty Types.SecretVersionStageType))
gsvrrsVersionStages = Lens.field @"versionStages"
{-# INLINEABLE gsvrrsVersionStages #-}
{-# DEPRECATED versionStages "Use generic-lens or generic-optics with 'versionStages' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsResponseStatus :: Lens.Lens' GetSecretValueResponse Core.Int
gsvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
