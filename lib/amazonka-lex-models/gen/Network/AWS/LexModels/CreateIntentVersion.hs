{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.CreateIntentVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of an intent based on the @> LATEST@ version of the intent. If the @> LATEST@ version of this intent hasn't changed since you last updated it, Amazon Lex doesn't create a new version. It returns the last version you created.
--
-- When you create a version of an intent, Amazon Lex sets the version to 1. Subsequent versions increment by 1. For more information, see 'versioning-intro' .
-- This operation requires permissions to perform the @lex:CreateIntentVersion@ action.
module Network.AWS.LexModels.CreateIntentVersion
  ( -- * Creating a request
    CreateIntentVersion (..),
    mkCreateIntentVersion,

    -- ** Request lenses
    civName,
    civChecksum,

    -- * Destructuring the response
    CreateIntentVersionResponse (..),
    mkCreateIntentVersionResponse,

    -- ** Response lenses
    civrrsChecksum,
    civrrsConclusionStatement,
    civrrsConfirmationPrompt,
    civrrsCreatedDate,
    civrrsDescription,
    civrrsDialogCodeHook,
    civrrsFollowUpPrompt,
    civrrsFulfillmentActivity,
    civrrsInputContexts,
    civrrsKendraConfiguration,
    civrrsLastUpdatedDate,
    civrrsName,
    civrrsOutputContexts,
    civrrsParentIntentSignature,
    civrrsRejectionStatement,
    civrrsSampleUtterances,
    civrrsSlots,
    civrrsVersion,
    civrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateIntentVersion' smart constructor.
data CreateIntentVersion = CreateIntentVersion'
  { -- | The name of the intent that you want to create a new version of. The name is case sensitive.
    name :: Types.IntentName,
    -- | Checksum of the @> LATEST@ version of the intent that should be used to create the new version. If you specify a checksum and the @> LATEST@ version of the intent has a different checksum, Amazon Lex returns a @PreconditionFailedException@ exception and doesn't publish a new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
    checksum :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateIntentVersion' value with any optional fields omitted.
mkCreateIntentVersion ::
  -- | 'name'
  Types.IntentName ->
  CreateIntentVersion
mkCreateIntentVersion name =
  CreateIntentVersion' {name, checksum = Core.Nothing}

-- | The name of the intent that you want to create a new version of. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civName :: Lens.Lens' CreateIntentVersion Types.IntentName
civName = Lens.field @"name"
{-# DEPRECATED civName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Checksum of the @> LATEST@ version of the intent that should be used to create the new version. If you specify a checksum and the @> LATEST@ version of the intent has a different checksum, Amazon Lex returns a @PreconditionFailedException@ exception and doesn't publish a new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civChecksum :: Lens.Lens' CreateIntentVersion (Core.Maybe Types.String)
civChecksum = Lens.field @"checksum"
{-# DEPRECATED civChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

instance Core.FromJSON CreateIntentVersion where
  toJSON CreateIntentVersion {..} =
    Core.object
      (Core.catMaybes [("checksum" Core..=) Core.<$> checksum])

instance Core.AWSRequest CreateIntentVersion where
  type Rs CreateIntentVersion = CreateIntentVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ("/intents/" Core.<> (Core.toText name) Core.<> ("/versions")),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIntentVersionResponse'
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

-- | /See:/ 'mkCreateIntentVersionResponse' smart constructor.
data CreateIntentVersionResponse = CreateIntentVersionResponse'
  { -- | Checksum of the intent version created.
    checksum :: Core.Maybe Types.String,
    -- | After the Lambda function specified in the @fulfillmentActivity@ field fulfills the intent, Amazon Lex conveys this statement to the user.
    conclusionStatement :: Core.Maybe Types.Statement,
    -- | If defined, the prompt that Amazon Lex uses to confirm the user's intent before fulfilling it.
    confirmationPrompt :: Core.Maybe Types.Prompt,
    -- | The date that the intent was created.
    createdDate :: Core.Maybe Core.NominalDiffTime,
    -- | A description of the intent.
    description :: Core.Maybe Types.Description,
    -- | If defined, Amazon Lex invokes this Lambda function for each user input.
    dialogCodeHook :: Core.Maybe Types.CodeHook,
    -- | If defined, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled.
    followUpPrompt :: Core.Maybe Types.FollowUpPrompt,
    -- | Describes how the intent is fulfilled.
    fulfillmentActivity :: Core.Maybe Types.FulfillmentActivity,
    -- | An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
    inputContexts :: Core.Maybe [Types.InputContext],
    -- | Configuration information, if any, for connecting an Amazon Kendra index with the @AMAZON.KendraSearchIntent@ intent.
    kendraConfiguration :: Core.Maybe Types.KendraConfiguration,
    -- | The date that the intent was updated.
    lastUpdatedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the intent.
    name :: Core.Maybe Types.IntentName,
    -- | An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
    outputContexts :: Core.Maybe [Types.OutputContext],
    -- | A unique identifier for a built-in intent.
    parentIntentSignature :: Core.Maybe Types.BuiltinIntentSignature,
    -- | If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
    rejectionStatement :: Core.Maybe Types.Statement,
    -- | An array of sample utterances configured for the intent.
    sampleUtterances :: Core.Maybe [Types.Utterance],
    -- | An array of slot types that defines the information required to fulfill the intent.
    slots :: Core.Maybe [Types.Slot],
    -- | The version number assigned to the new version of the intent.
    version :: Core.Maybe Types.Version,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateIntentVersionResponse' value with any optional fields omitted.
mkCreateIntentVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateIntentVersionResponse
mkCreateIntentVersionResponse responseStatus =
  CreateIntentVersionResponse'
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

-- | Checksum of the intent version created.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsChecksum :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Types.String)
civrrsChecksum = Lens.field @"checksum"
{-# DEPRECATED civrrsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | After the Lambda function specified in the @fulfillmentActivity@ field fulfills the intent, Amazon Lex conveys this statement to the user.
--
-- /Note:/ Consider using 'conclusionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsConclusionStatement :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Types.Statement)
civrrsConclusionStatement = Lens.field @"conclusionStatement"
{-# DEPRECATED civrrsConclusionStatement "Use generic-lens or generic-optics with 'conclusionStatement' instead." #-}

-- | If defined, the prompt that Amazon Lex uses to confirm the user's intent before fulfilling it.
--
-- /Note:/ Consider using 'confirmationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsConfirmationPrompt :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Types.Prompt)
civrrsConfirmationPrompt = Lens.field @"confirmationPrompt"
{-# DEPRECATED civrrsConfirmationPrompt "Use generic-lens or generic-optics with 'confirmationPrompt' instead." #-}

-- | The date that the intent was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsCreatedDate :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Core.NominalDiffTime)
civrrsCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED civrrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | A description of the intent.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsDescription :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Types.Description)
civrrsDescription = Lens.field @"description"
{-# DEPRECATED civrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | If defined, Amazon Lex invokes this Lambda function for each user input.
--
-- /Note:/ Consider using 'dialogCodeHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsDialogCodeHook :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Types.CodeHook)
civrrsDialogCodeHook = Lens.field @"dialogCodeHook"
{-# DEPRECATED civrrsDialogCodeHook "Use generic-lens or generic-optics with 'dialogCodeHook' instead." #-}

-- | If defined, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled.
--
-- /Note:/ Consider using 'followUpPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsFollowUpPrompt :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Types.FollowUpPrompt)
civrrsFollowUpPrompt = Lens.field @"followUpPrompt"
{-# DEPRECATED civrrsFollowUpPrompt "Use generic-lens or generic-optics with 'followUpPrompt' instead." #-}

-- | Describes how the intent is fulfilled.
--
-- /Note:/ Consider using 'fulfillmentActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsFulfillmentActivity :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Types.FulfillmentActivity)
civrrsFulfillmentActivity = Lens.field @"fulfillmentActivity"
{-# DEPRECATED civrrsFulfillmentActivity "Use generic-lens or generic-optics with 'fulfillmentActivity' instead." #-}

-- | An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
--
-- /Note:/ Consider using 'inputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsInputContexts :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe [Types.InputContext])
civrrsInputContexts = Lens.field @"inputContexts"
{-# DEPRECATED civrrsInputContexts "Use generic-lens or generic-optics with 'inputContexts' instead." #-}

-- | Configuration information, if any, for connecting an Amazon Kendra index with the @AMAZON.KendraSearchIntent@ intent.
--
-- /Note:/ Consider using 'kendraConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsKendraConfiguration :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Types.KendraConfiguration)
civrrsKendraConfiguration = Lens.field @"kendraConfiguration"
{-# DEPRECATED civrrsKendraConfiguration "Use generic-lens or generic-optics with 'kendraConfiguration' instead." #-}

-- | The date that the intent was updated.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsLastUpdatedDate :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Core.NominalDiffTime)
civrrsLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# DEPRECATED civrrsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | The name of the intent.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsName :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Types.IntentName)
civrrsName = Lens.field @"name"
{-# DEPRECATED civrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
--
-- /Note:/ Consider using 'outputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsOutputContexts :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe [Types.OutputContext])
civrrsOutputContexts = Lens.field @"outputContexts"
{-# DEPRECATED civrrsOutputContexts "Use generic-lens or generic-optics with 'outputContexts' instead." #-}

-- | A unique identifier for a built-in intent.
--
-- /Note:/ Consider using 'parentIntentSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsParentIntentSignature :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Types.BuiltinIntentSignature)
civrrsParentIntentSignature = Lens.field @"parentIntentSignature"
{-# DEPRECATED civrrsParentIntentSignature "Use generic-lens or generic-optics with 'parentIntentSignature' instead." #-}

-- | If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
--
-- /Note:/ Consider using 'rejectionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsRejectionStatement :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Types.Statement)
civrrsRejectionStatement = Lens.field @"rejectionStatement"
{-# DEPRECATED civrrsRejectionStatement "Use generic-lens or generic-optics with 'rejectionStatement' instead." #-}

-- | An array of sample utterances configured for the intent.
--
-- /Note:/ Consider using 'sampleUtterances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsSampleUtterances :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe [Types.Utterance])
civrrsSampleUtterances = Lens.field @"sampleUtterances"
{-# DEPRECATED civrrsSampleUtterances "Use generic-lens or generic-optics with 'sampleUtterances' instead." #-}

-- | An array of slot types that defines the information required to fulfill the intent.
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsSlots :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe [Types.Slot])
civrrsSlots = Lens.field @"slots"
{-# DEPRECATED civrrsSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | The version number assigned to the new version of the intent.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsVersion :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Types.Version)
civrrsVersion = Lens.field @"version"
{-# DEPRECATED civrrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsResponseStatus :: Lens.Lens' CreateIntentVersionResponse Core.Int
civrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED civrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
