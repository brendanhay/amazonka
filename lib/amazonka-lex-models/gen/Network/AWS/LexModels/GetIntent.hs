{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetIntent (..),
    mkGetIntent,

    -- ** Request lenses
    giName,
    giVersion,

    -- * Destructuring the response
    GetIntentResponse (..),
    mkGetIntentResponse,

    -- ** Response lenses
    girfrsChecksum,
    girfrsConclusionStatement,
    girfrsConfirmationPrompt,
    girfrsCreatedDate,
    girfrsDescription,
    girfrsDialogCodeHook,
    girfrsFollowUpPrompt,
    girfrsFulfillmentActivity,
    girfrsInputContexts,
    girfrsKendraConfiguration,
    girfrsLastUpdatedDate,
    girfrsName,
    girfrsOutputContexts,
    girfrsParentIntentSignature,
    girfrsRejectionStatement,
    girfrsSampleUtterances,
    girfrsSlots,
    girfrsVersion,
    girfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetIntent' smart constructor.
data GetIntent = GetIntent'
  { -- | The name of the intent. The name is case sensitive.
    name :: Types.IntentName,
    -- | The version of the intent.
    version :: Types.Version
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIntent' value with any optional fields omitted.
mkGetIntent ::
  -- | 'name'
  Types.IntentName ->
  -- | 'version'
  Types.Version ->
  GetIntent
mkGetIntent name version = GetIntent' {name, version}

-- | The name of the intent. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giName :: Lens.Lens' GetIntent Types.IntentName
giName = Lens.field @"name"
{-# DEPRECATED giName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the intent.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giVersion :: Lens.Lens' GetIntent Types.Version
giVersion = Lens.field @"version"
{-# DEPRECATED giVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.AWSRequest GetIntent where
  type Rs GetIntent = GetIntentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/intents/" Core.<> (Core.toText name) Core.<> ("/versions/")
                Core.<> (Core.toText version)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIntentResponse'
            Core.<$> (x Core..:? "checksum")
            Core.<*> (x Core..:? "conclusionStatement")
            Core.<*> (x Core..:? "confirmationPrompt")
            Core.<*> (x Core..:? "createdDate")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "dialogCodeHook")
            Core.<*> (x Core..:? "followUpPrompt")
            Core.<*> (x Core..:? "fulfillmentActivity")
            Core.<*> (x Core..:? "inputContexts")
            Core.<*> (x Core..:? "kendraConfiguration")
            Core.<*> (x Core..:? "lastUpdatedDate")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "outputContexts")
            Core.<*> (x Core..:? "parentIntentSignature")
            Core.<*> (x Core..:? "rejectionStatement")
            Core.<*> (x Core..:? "sampleUtterances")
            Core.<*> (x Core..:? "slots")
            Core.<*> (x Core..:? "version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetIntentResponse' smart constructor.
data GetIntentResponse = GetIntentResponse'
  { -- | Checksum of the intent.
    checksum :: Core.Maybe Types.String,
    -- | After the Lambda function specified in the @fulfillmentActivity@ element fulfills the intent, Amazon Lex conveys this statement to the user.
    conclusionStatement :: Core.Maybe Types.Statement,
    -- | If defined in the bot, Amazon Lex uses prompt to confirm the intent before fulfilling the user's request. For more information, see 'PutIntent' .
    confirmationPrompt :: Core.Maybe Types.Prompt,
    -- | The date that the intent was created.
    createdDate :: Core.Maybe Core.NominalDiffTime,
    -- | A description of the intent.
    description :: Core.Maybe Types.Description,
    -- | If defined in the bot, Amazon Amazon Lex invokes this Lambda function for each user input. For more information, see 'PutIntent' .
    dialogCodeHook :: Core.Maybe Types.CodeHook,
    -- | If defined in the bot, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled. For more information, see 'PutIntent' .
    followUpPrompt :: Core.Maybe Types.FollowUpPrompt,
    -- | Describes how the intent is fulfilled. For more information, see 'PutIntent' .
    fulfillmentActivity :: Core.Maybe Types.FulfillmentActivity,
    -- | An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
    inputContexts :: Core.Maybe [Types.InputContext],
    -- | Configuration information, if any, to connect to an Amazon Kendra index with the @AMAZON.KendraSearchIntent@ intent.
    kendraConfiguration :: Core.Maybe Types.KendraConfiguration,
    -- | The date that the intent was updated. When you create a resource, the creation date and the last updated date are the same.
    lastUpdatedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the intent.
    name :: Core.Maybe Types.IntentName,
    -- | An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
    outputContexts :: Core.Maybe [Types.OutputContext],
    -- | A unique identifier for a built-in intent.
    parentIntentSignature :: Core.Maybe Types.ParentIntentSignature,
    -- | If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
    rejectionStatement :: Core.Maybe Types.Statement,
    -- | An array of sample utterances configured for the intent.
    sampleUtterances :: Core.Maybe [Types.Utterance],
    -- | An array of intent slots configured for the intent.
    slots :: Core.Maybe [Types.Slot],
    -- | The version of the intent.
    version :: Core.Maybe Types.Version,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetIntentResponse' value with any optional fields omitted.
mkGetIntentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetIntentResponse
mkGetIntentResponse responseStatus =
  GetIntentResponse'
    { checksum = Core.Nothing,
      conclusionStatement = Core.Nothing,
      confirmationPrompt = Core.Nothing,
      createdDate = Core.Nothing,
      description = Core.Nothing,
      dialogCodeHook = Core.Nothing,
      followUpPrompt = Core.Nothing,
      fulfillmentActivity = Core.Nothing,
      inputContexts = Core.Nothing,
      kendraConfiguration = Core.Nothing,
      lastUpdatedDate = Core.Nothing,
      name = Core.Nothing,
      outputContexts = Core.Nothing,
      parentIntentSignature = Core.Nothing,
      rejectionStatement = Core.Nothing,
      sampleUtterances = Core.Nothing,
      slots = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | Checksum of the intent.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsChecksum :: Lens.Lens' GetIntentResponse (Core.Maybe Types.String)
girfrsChecksum = Lens.field @"checksum"
{-# DEPRECATED girfrsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | After the Lambda function specified in the @fulfillmentActivity@ element fulfills the intent, Amazon Lex conveys this statement to the user.
--
-- /Note:/ Consider using 'conclusionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsConclusionStatement :: Lens.Lens' GetIntentResponse (Core.Maybe Types.Statement)
girfrsConclusionStatement = Lens.field @"conclusionStatement"
{-# DEPRECATED girfrsConclusionStatement "Use generic-lens or generic-optics with 'conclusionStatement' instead." #-}

-- | If defined in the bot, Amazon Lex uses prompt to confirm the intent before fulfilling the user's request. For more information, see 'PutIntent' .
--
-- /Note:/ Consider using 'confirmationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsConfirmationPrompt :: Lens.Lens' GetIntentResponse (Core.Maybe Types.Prompt)
girfrsConfirmationPrompt = Lens.field @"confirmationPrompt"
{-# DEPRECATED girfrsConfirmationPrompt "Use generic-lens or generic-optics with 'confirmationPrompt' instead." #-}

-- | The date that the intent was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsCreatedDate :: Lens.Lens' GetIntentResponse (Core.Maybe Core.NominalDiffTime)
girfrsCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED girfrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | A description of the intent.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsDescription :: Lens.Lens' GetIntentResponse (Core.Maybe Types.Description)
girfrsDescription = Lens.field @"description"
{-# DEPRECATED girfrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | If defined in the bot, Amazon Amazon Lex invokes this Lambda function for each user input. For more information, see 'PutIntent' .
--
-- /Note:/ Consider using 'dialogCodeHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsDialogCodeHook :: Lens.Lens' GetIntentResponse (Core.Maybe Types.CodeHook)
girfrsDialogCodeHook = Lens.field @"dialogCodeHook"
{-# DEPRECATED girfrsDialogCodeHook "Use generic-lens or generic-optics with 'dialogCodeHook' instead." #-}

-- | If defined in the bot, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled. For more information, see 'PutIntent' .
--
-- /Note:/ Consider using 'followUpPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsFollowUpPrompt :: Lens.Lens' GetIntentResponse (Core.Maybe Types.FollowUpPrompt)
girfrsFollowUpPrompt = Lens.field @"followUpPrompt"
{-# DEPRECATED girfrsFollowUpPrompt "Use generic-lens or generic-optics with 'followUpPrompt' instead." #-}

-- | Describes how the intent is fulfilled. For more information, see 'PutIntent' .
--
-- /Note:/ Consider using 'fulfillmentActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsFulfillmentActivity :: Lens.Lens' GetIntentResponse (Core.Maybe Types.FulfillmentActivity)
girfrsFulfillmentActivity = Lens.field @"fulfillmentActivity"
{-# DEPRECATED girfrsFulfillmentActivity "Use generic-lens or generic-optics with 'fulfillmentActivity' instead." #-}

-- | An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
--
-- /Note:/ Consider using 'inputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsInputContexts :: Lens.Lens' GetIntentResponse (Core.Maybe [Types.InputContext])
girfrsInputContexts = Lens.field @"inputContexts"
{-# DEPRECATED girfrsInputContexts "Use generic-lens or generic-optics with 'inputContexts' instead." #-}

-- | Configuration information, if any, to connect to an Amazon Kendra index with the @AMAZON.KendraSearchIntent@ intent.
--
-- /Note:/ Consider using 'kendraConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsKendraConfiguration :: Lens.Lens' GetIntentResponse (Core.Maybe Types.KendraConfiguration)
girfrsKendraConfiguration = Lens.field @"kendraConfiguration"
{-# DEPRECATED girfrsKendraConfiguration "Use generic-lens or generic-optics with 'kendraConfiguration' instead." #-}

-- | The date that the intent was updated. When you create a resource, the creation date and the last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsLastUpdatedDate :: Lens.Lens' GetIntentResponse (Core.Maybe Core.NominalDiffTime)
girfrsLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# DEPRECATED girfrsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | The name of the intent.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsName :: Lens.Lens' GetIntentResponse (Core.Maybe Types.IntentName)
girfrsName = Lens.field @"name"
{-# DEPRECATED girfrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
--
-- /Note:/ Consider using 'outputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsOutputContexts :: Lens.Lens' GetIntentResponse (Core.Maybe [Types.OutputContext])
girfrsOutputContexts = Lens.field @"outputContexts"
{-# DEPRECATED girfrsOutputContexts "Use generic-lens or generic-optics with 'outputContexts' instead." #-}

-- | A unique identifier for a built-in intent.
--
-- /Note:/ Consider using 'parentIntentSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsParentIntentSignature :: Lens.Lens' GetIntentResponse (Core.Maybe Types.ParentIntentSignature)
girfrsParentIntentSignature = Lens.field @"parentIntentSignature"
{-# DEPRECATED girfrsParentIntentSignature "Use generic-lens or generic-optics with 'parentIntentSignature' instead." #-}

-- | If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
--
-- /Note:/ Consider using 'rejectionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsRejectionStatement :: Lens.Lens' GetIntentResponse (Core.Maybe Types.Statement)
girfrsRejectionStatement = Lens.field @"rejectionStatement"
{-# DEPRECATED girfrsRejectionStatement "Use generic-lens or generic-optics with 'rejectionStatement' instead." #-}

-- | An array of sample utterances configured for the intent.
--
-- /Note:/ Consider using 'sampleUtterances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsSampleUtterances :: Lens.Lens' GetIntentResponse (Core.Maybe [Types.Utterance])
girfrsSampleUtterances = Lens.field @"sampleUtterances"
{-# DEPRECATED girfrsSampleUtterances "Use generic-lens or generic-optics with 'sampleUtterances' instead." #-}

-- | An array of intent slots configured for the intent.
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsSlots :: Lens.Lens' GetIntentResponse (Core.Maybe [Types.Slot])
girfrsSlots = Lens.field @"slots"
{-# DEPRECATED girfrsSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | The version of the intent.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsVersion :: Lens.Lens' GetIntentResponse (Core.Maybe Types.Version)
girfrsVersion = Lens.field @"version"
{-# DEPRECATED girfrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girfrsResponseStatus :: Lens.Lens' GetIntentResponse Core.Int
girfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED girfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
