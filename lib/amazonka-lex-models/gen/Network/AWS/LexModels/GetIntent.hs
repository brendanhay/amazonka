{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetIntent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an intent. In addition to the intent name, you must specify the intent version. 
--
-- This operation requires permissions to perform the @lex:GetIntent@ action. 
module Network.AWS.LexModels.GetIntent
    (
    -- * Creating a request
      GetIntent (..)
    , mkGetIntent
    -- ** Request lenses
    , giName
    , giVersion

    -- * Destructuring the response
    , GetIntentResponse (..)
    , mkGetIntentResponse
    -- ** Response lenses
    , girfrsChecksum
    , girfrsConclusionStatement
    , girfrsConfirmationPrompt
    , girfrsCreatedDate
    , girfrsDescription
    , girfrsDialogCodeHook
    , girfrsFollowUpPrompt
    , girfrsFulfillmentActivity
    , girfrsInputContexts
    , girfrsKendraConfiguration
    , girfrsLastUpdatedDate
    , girfrsName
    , girfrsOutputContexts
    , girfrsParentIntentSignature
    , girfrsRejectionStatement
    , girfrsSampleUtterances
    , girfrsSlots
    , girfrsVersion
    , girfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetIntent' smart constructor.
data GetIntent = GetIntent'
  { name :: Types.IntentName
    -- ^ The name of the intent. The name is case sensitive. 
  , version :: Types.Version
    -- ^ The version of the intent.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIntent' value with any optional fields omitted.
mkGetIntent
    :: Types.IntentName -- ^ 'name'
    -> Types.Version -- ^ 'version'
    -> GetIntent
mkGetIntent name version = GetIntent'{name, version}

-- | The name of the intent. The name is case sensitive. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giName :: Lens.Lens' GetIntent Types.IntentName
giName = Lens.field @"name"
{-# INLINEABLE giName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The version of the intent.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giVersion :: Lens.Lens' GetIntent Types.Version
giVersion = Lens.field @"version"
{-# INLINEABLE giVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.ToQuery GetIntent where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetIntent where
        toHeaders GetIntent{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetIntent where
        type Rs GetIntent = GetIntentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/intents/" Core.<> Core.toText name Core.<> "/versions/" Core.<>
                             Core.toText version,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetIntentResponse' Core.<$>
                   (x Core..:? "checksum") Core.<*> x Core..:? "conclusionStatement"
                     Core.<*> x Core..:? "confirmationPrompt"
                     Core.<*> x Core..:? "createdDate"
                     Core.<*> x Core..:? "description"
                     Core.<*> x Core..:? "dialogCodeHook"
                     Core.<*> x Core..:? "followUpPrompt"
                     Core.<*> x Core..:? "fulfillmentActivity"
                     Core.<*> x Core..:? "inputContexts"
                     Core.<*> x Core..:? "kendraConfiguration"
                     Core.<*> x Core..:? "lastUpdatedDate"
                     Core.<*> x Core..:? "name"
                     Core.<*> x Core..:? "outputContexts"
                     Core.<*> x Core..:? "parentIntentSignature"
                     Core.<*> x Core..:? "rejectionStatement"
                     Core.<*> x Core..:? "sampleUtterances"
                     Core.<*> x Core..:? "slots"
                     Core.<*> x Core..:? "version"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetIntentResponse' smart constructor.
data GetIntentResponse = GetIntentResponse'
  { checksum :: Core.Maybe Core.Text
    -- ^ Checksum of the intent.
  , conclusionStatement :: Core.Maybe Types.Statement
    -- ^ After the Lambda function specified in the @fulfillmentActivity@ element fulfills the intent, Amazon Lex conveys this statement to the user.
  , confirmationPrompt :: Core.Maybe Types.Prompt
    -- ^ If defined in the bot, Amazon Lex uses prompt to confirm the intent before fulfilling the user's request. For more information, see 'PutIntent' . 
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the intent was created.
  , description :: Core.Maybe Types.Description
    -- ^ A description of the intent.
  , dialogCodeHook :: Core.Maybe Types.CodeHook
    -- ^ If defined in the bot, Amazon Amazon Lex invokes this Lambda function for each user input. For more information, see 'PutIntent' . 
  , followUpPrompt :: Core.Maybe Types.FollowUpPrompt
    -- ^ If defined in the bot, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled. For more information, see 'PutIntent' .
  , fulfillmentActivity :: Core.Maybe Types.FulfillmentActivity
    -- ^ Describes how the intent is fulfilled. For more information, see 'PutIntent' . 
  , inputContexts :: Core.Maybe [Types.InputContext]
    -- ^ An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
  , kendraConfiguration :: Core.Maybe Types.KendraConfiguration
    -- ^ Configuration information, if any, to connect to an Amazon Kendra index with the @AMAZON.KendraSearchIntent@ intent.
  , lastUpdatedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the intent was updated. When you create a resource, the creation date and the last updated date are the same. 
  , name :: Core.Maybe Types.IntentName
    -- ^ The name of the intent.
  , outputContexts :: Core.Maybe [Types.OutputContext]
    -- ^ An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
  , parentIntentSignature :: Core.Maybe Types.ParentIntentSignature
    -- ^ A unique identifier for a built-in intent.
  , rejectionStatement :: Core.Maybe Types.Statement
    -- ^ If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled. 
  , sampleUtterances :: Core.Maybe [Types.Utterance]
    -- ^ An array of sample utterances configured for the intent.
  , slots :: Core.Maybe [Types.Slot]
    -- ^ An array of intent slots configured for the intent.
  , version :: Core.Maybe Types.Version
    -- ^ The version of the intent.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetIntentResponse' value with any optional fields omitted.
mkGetIntentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetIntentResponse
mkGetIntentResponse responseStatus
  = GetIntentResponse'{checksum = Core.Nothing,
                       conclusionStatement = Core.Nothing,
                       confirmationPrompt = Core.Nothing, createdDate = Core.Nothing,
                       description = Core.Nothing, dialogCodeHook = Core.Nothing,
                       followUpPrompt = Core.Nothing, fulfillmentActivity = Core.Nothing,
                       inputContexts = Core.Nothing, kendraConfiguration = Core.Nothing,
                       lastUpdatedDate = Core.Nothing, name = Core.Nothing,
                       outputContexts = Core.Nothing,
                       parentIntentSignature = Core.Nothing,
                       rejectionStatement = Core.Nothing, sampleUtterances = Core.Nothing,
                       slots = Core.Nothing, version = Core.Nothing, responseStatus}

-- | Checksum of the intent.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsChecksum :: Lens.Lens' GetIntentResponse (Core.Maybe Core.Text)
girfrsChecksum = Lens.field @"checksum"
{-# INLINEABLE girfrsChecksum #-}
{-# DEPRECATED checksum "Use generic-lens or generic-optics with 'checksum' instead"  #-}

-- | After the Lambda function specified in the @fulfillmentActivity@ element fulfills the intent, Amazon Lex conveys this statement to the user.
--
-- /Note:/ Consider using 'conclusionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsConclusionStatement :: Lens.Lens' GetIntentResponse (Core.Maybe Types.Statement)
girfrsConclusionStatement = Lens.field @"conclusionStatement"
{-# INLINEABLE girfrsConclusionStatement #-}
{-# DEPRECATED conclusionStatement "Use generic-lens or generic-optics with 'conclusionStatement' instead"  #-}

-- | If defined in the bot, Amazon Lex uses prompt to confirm the intent before fulfilling the user's request. For more information, see 'PutIntent' . 
--
-- /Note:/ Consider using 'confirmationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsConfirmationPrompt :: Lens.Lens' GetIntentResponse (Core.Maybe Types.Prompt)
girfrsConfirmationPrompt = Lens.field @"confirmationPrompt"
{-# INLINEABLE girfrsConfirmationPrompt #-}
{-# DEPRECATED confirmationPrompt "Use generic-lens or generic-optics with 'confirmationPrompt' instead"  #-}

-- | The date that the intent was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsCreatedDate :: Lens.Lens' GetIntentResponse (Core.Maybe Core.NominalDiffTime)
girfrsCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE girfrsCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | A description of the intent.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsDescription :: Lens.Lens' GetIntentResponse (Core.Maybe Types.Description)
girfrsDescription = Lens.field @"description"
{-# INLINEABLE girfrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | If defined in the bot, Amazon Amazon Lex invokes this Lambda function for each user input. For more information, see 'PutIntent' . 
--
-- /Note:/ Consider using 'dialogCodeHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsDialogCodeHook :: Lens.Lens' GetIntentResponse (Core.Maybe Types.CodeHook)
girfrsDialogCodeHook = Lens.field @"dialogCodeHook"
{-# INLINEABLE girfrsDialogCodeHook #-}
{-# DEPRECATED dialogCodeHook "Use generic-lens or generic-optics with 'dialogCodeHook' instead"  #-}

-- | If defined in the bot, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled. For more information, see 'PutIntent' .
--
-- /Note:/ Consider using 'followUpPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsFollowUpPrompt :: Lens.Lens' GetIntentResponse (Core.Maybe Types.FollowUpPrompt)
girfrsFollowUpPrompt = Lens.field @"followUpPrompt"
{-# INLINEABLE girfrsFollowUpPrompt #-}
{-# DEPRECATED followUpPrompt "Use generic-lens or generic-optics with 'followUpPrompt' instead"  #-}

-- | Describes how the intent is fulfilled. For more information, see 'PutIntent' . 
--
-- /Note:/ Consider using 'fulfillmentActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsFulfillmentActivity :: Lens.Lens' GetIntentResponse (Core.Maybe Types.FulfillmentActivity)
girfrsFulfillmentActivity = Lens.field @"fulfillmentActivity"
{-# INLINEABLE girfrsFulfillmentActivity #-}
{-# DEPRECATED fulfillmentActivity "Use generic-lens or generic-optics with 'fulfillmentActivity' instead"  #-}

-- | An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
--
-- /Note:/ Consider using 'inputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsInputContexts :: Lens.Lens' GetIntentResponse (Core.Maybe [Types.InputContext])
girfrsInputContexts = Lens.field @"inputContexts"
{-# INLINEABLE girfrsInputContexts #-}
{-# DEPRECATED inputContexts "Use generic-lens or generic-optics with 'inputContexts' instead"  #-}

-- | Configuration information, if any, to connect to an Amazon Kendra index with the @AMAZON.KendraSearchIntent@ intent.
--
-- /Note:/ Consider using 'kendraConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsKendraConfiguration :: Lens.Lens' GetIntentResponse (Core.Maybe Types.KendraConfiguration)
girfrsKendraConfiguration = Lens.field @"kendraConfiguration"
{-# INLINEABLE girfrsKendraConfiguration #-}
{-# DEPRECATED kendraConfiguration "Use generic-lens or generic-optics with 'kendraConfiguration' instead"  #-}

-- | The date that the intent was updated. When you create a resource, the creation date and the last updated date are the same. 
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsLastUpdatedDate :: Lens.Lens' GetIntentResponse (Core.Maybe Core.NominalDiffTime)
girfrsLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# INLINEABLE girfrsLastUpdatedDate #-}
{-# DEPRECATED lastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead"  #-}

-- | The name of the intent.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsName :: Lens.Lens' GetIntentResponse (Core.Maybe Types.IntentName)
girfrsName = Lens.field @"name"
{-# INLINEABLE girfrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
--
-- /Note:/ Consider using 'outputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsOutputContexts :: Lens.Lens' GetIntentResponse (Core.Maybe [Types.OutputContext])
girfrsOutputContexts = Lens.field @"outputContexts"
{-# INLINEABLE girfrsOutputContexts #-}
{-# DEPRECATED outputContexts "Use generic-lens or generic-optics with 'outputContexts' instead"  #-}

-- | A unique identifier for a built-in intent.
--
-- /Note:/ Consider using 'parentIntentSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsParentIntentSignature :: Lens.Lens' GetIntentResponse (Core.Maybe Types.ParentIntentSignature)
girfrsParentIntentSignature = Lens.field @"parentIntentSignature"
{-# INLINEABLE girfrsParentIntentSignature #-}
{-# DEPRECATED parentIntentSignature "Use generic-lens or generic-optics with 'parentIntentSignature' instead"  #-}

-- | If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled. 
--
-- /Note:/ Consider using 'rejectionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsRejectionStatement :: Lens.Lens' GetIntentResponse (Core.Maybe Types.Statement)
girfrsRejectionStatement = Lens.field @"rejectionStatement"
{-# INLINEABLE girfrsRejectionStatement #-}
{-# DEPRECATED rejectionStatement "Use generic-lens or generic-optics with 'rejectionStatement' instead"  #-}

-- | An array of sample utterances configured for the intent.
--
-- /Note:/ Consider using 'sampleUtterances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsSampleUtterances :: Lens.Lens' GetIntentResponse (Core.Maybe [Types.Utterance])
girfrsSampleUtterances = Lens.field @"sampleUtterances"
{-# INLINEABLE girfrsSampleUtterances #-}
{-# DEPRECATED sampleUtterances "Use generic-lens or generic-optics with 'sampleUtterances' instead"  #-}

-- | An array of intent slots configured for the intent.
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsSlots :: Lens.Lens' GetIntentResponse (Core.Maybe [Types.Slot])
girfrsSlots = Lens.field @"slots"
{-# INLINEABLE girfrsSlots #-}
{-# DEPRECATED slots "Use generic-lens or generic-optics with 'slots' instead"  #-}

-- | The version of the intent.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsVersion :: Lens.Lens' GetIntentResponse (Core.Maybe Types.Version)
girfrsVersion = Lens.field @"version"
{-# INLINEABLE girfrsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsResponseStatus :: Lens.Lens' GetIntentResponse Core.Int
girfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE girfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
