{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.PutIntent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an intent or replaces an existing intent.
--
-- To define the interaction between the user and your bot, you use one or more intents. For a pizza ordering bot, for example, you would create an @OrderPizza@ intent. 
-- To create an intent or replace an existing intent, you must provide the following:
--
--     * Intent name. For example, @OrderPizza@ .
--
--
--     * Sample utterances. For example, "Can I order a pizza, please." and "I want to order a pizza."
--
--
--     * Information to be gathered. You specify slot types for the information that your bot will request from the user. You can specify standard slot types, such as a date or a time, or custom slot types such as the size and crust of a pizza.
--
--
--     * How the intent will be fulfilled. You can provide a Lambda function or configure the intent to return the intent information to the client application. If you use a Lambda function, when all of the intent information is available, Amazon Lex invokes your Lambda function. If you configure your intent to return the intent information to the client application. 
--
--
-- You can specify other optional information in the request, such as:
--
--     * A confirmation prompt to ask the user to confirm an intent. For example, "Shall I order your pizza?"
--
--
--     * A conclusion statement to send to the user after the intent has been fulfilled. For example, "I placed your pizza order."
--
--
--     * A follow-up prompt that asks the user for additional activity. For example, asking "Do you want to order a drink with your pizza?"
--
--
-- If you specify an existing intent name to update the intent, Amazon Lex replaces the values in the @> LATEST@ version of the intent with the values in the request. Amazon Lex removes fields that you don't provide in the request. If you don't specify the required fields, Amazon Lex throws an exception. When you update the @> LATEST@ version of an intent, the @status@ field of any bot that uses the @> LATEST@ version of the intent is set to @NOT_BUILT@ .
-- For more information, see 'how-it-works' .
-- This operation requires permissions for the @lex:PutIntent@ action.
module Network.AWS.LexModels.PutIntent
    (
    -- * Creating a request
      PutIntent (..)
    , mkPutIntent
    -- ** Request lenses
    , piName
    , piChecksum
    , piConclusionStatement
    , piConfirmationPrompt
    , piCreateVersion
    , piDescription
    , piDialogCodeHook
    , piFollowUpPrompt
    , piFulfillmentActivity
    , piInputContexts
    , piKendraConfiguration
    , piOutputContexts
    , piParentIntentSignature
    , piRejectionStatement
    , piSampleUtterances
    , piSlots

    -- * Destructuring the response
    , PutIntentResponse (..)
    , mkPutIntentResponse
    -- ** Response lenses
    , pirrsChecksum
    , pirrsConclusionStatement
    , pirrsConfirmationPrompt
    , pirrsCreateVersion
    , pirrsCreatedDate
    , pirrsDescription
    , pirrsDialogCodeHook
    , pirrsFollowUpPrompt
    , pirrsFulfillmentActivity
    , pirrsInputContexts
    , pirrsKendraConfiguration
    , pirrsLastUpdatedDate
    , pirrsName
    , pirrsOutputContexts
    , pirrsParentIntentSignature
    , pirrsRejectionStatement
    , pirrsSampleUtterances
    , pirrsSlots
    , pirrsVersion
    , pirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutIntent' smart constructor.
data PutIntent = PutIntent'
  { name :: Types.IntentName
    -- ^ The name of the intent. The name is /not/ case sensitive. 
--
-- The name can't match a built-in intent name, or a built-in intent name with "AMAZON." removed. For example, because there is a built-in intent called @AMAZON.HelpIntent@ , you can't create a custom intent called @HelpIntent@ .
-- For a list of built-in intents, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
  , checksum :: Core.Maybe Core.Text
    -- ^ Identifies a specific revision of the @> LATEST@ version.
--
-- When you create a new intent, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception.
-- When you want to update a intent, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
  , conclusionStatement :: Core.Maybe Types.Statement
    -- ^ The statement that you want Amazon Lex to convey to the user after the intent is successfully fulfilled by the Lambda function. 
--
-- This element is relevant only if you provide a Lambda function in the @fulfillmentActivity@ . If you return the intent to the client application, you can't specify this element.
  , confirmationPrompt :: Core.Maybe Types.Prompt
    -- ^ Prompts the user to confirm the intent. This question should have a yes or no answer.
--
-- Amazon Lex uses this prompt to ensure that the user acknowledges that the intent is ready for fulfillment. For example, with the @OrderPizza@ intent, you might want to confirm that the order is correct before placing it. For other intents, such as intents that simply respond to user questions, you might not need to ask the user for confirmation before providing the information. 
  , createVersion :: Core.Maybe Core.Bool
    -- ^ When set to @true@ a new numbered version of the intent is created. This is the same as calling the @CreateIntentVersion@ operation. If you do not specify @createVersion@ , the default is @false@ .
  , description :: Core.Maybe Types.Description
    -- ^ A description of the intent.
  , dialogCodeHook :: Core.Maybe Types.CodeHook
    -- ^ Specifies a Lambda function to invoke for each user input. You can invoke this Lambda function to personalize user interaction. 
--
-- For example, suppose your bot determines that the user is John. Your Lambda function might retrieve John's information from a backend database and prepopulate some of the values. For example, if you find that John is gluten intolerant, you might set the corresponding intent slot, @GlutenIntolerant@ , to true. You might find John's phone number and set the corresponding session attribute. 
  , followUpPrompt :: Core.Maybe Types.FollowUpPrompt
    -- ^ Amazon Lex uses this prompt to solicit additional activity after fulfilling an intent. For example, after the @OrderPizza@ intent is fulfilled, you might prompt the user to order a drink.
--
-- The action that Amazon Lex takes depends on the user's response, as follows:
--
--     * If the user says "Yes" it responds with the clarification prompt that is configured for the bot.
--
--
--     * if the user says "Yes" and continues with an utterance that triggers an intent it starts a conversation for the intent.
--
--
--     * If the user says "No" it responds with the rejection statement configured for the the follow-up prompt.
--
--
--     * If it doesn't recognize the utterance it repeats the follow-up prompt again.
--
--
-- The @followUpPrompt@ field and the @conclusionStatement@ field are mutually exclusive. You can specify only one. 
  , fulfillmentActivity :: Core.Maybe Types.FulfillmentActivity
    -- ^ Required. Describes how the intent is fulfilled. For example, after a user provides all of the information for a pizza order, @fulfillmentActivity@ defines how the bot places an order with a local pizza store. 
--
-- You might configure Amazon Lex to return all of the intent information to the client application, or direct it to invoke a Lambda function that can process the intent (for example, place an order with a pizzeria). 
  , inputContexts :: Core.Maybe [Types.InputContext]
    -- ^ An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
  , kendraConfiguration :: Core.Maybe Types.KendraConfiguration
    -- ^ Configuration information required to use the @AMAZON.KendraSearchIntent@ intent to connect to an Amazon Kendra index. For more information, see <http://docs.aws.amazon.com/lex/latest/dg/built-in-intent-kendra-search.html AMAZON.KendraSearchIntent> .
  , outputContexts :: Core.Maybe [Types.OutputContext]
    -- ^ An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
  , parentIntentSignature :: Core.Maybe Types.ParentIntentSignature
    -- ^ A unique identifier for the built-in intent to base this intent on. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
  , rejectionStatement :: Core.Maybe Types.Statement
    -- ^ When the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled. 
  , sampleUtterances :: Core.Maybe [Types.Utterance]
    -- ^ An array of utterances (strings) that a user might say to signal the intent. For example, "I want {PizzaSize} pizza", "Order {Quantity} {PizzaSize} pizzas". 
--
-- In each utterance, a slot name is enclosed in curly braces. 
  , slots :: Core.Maybe [Types.Slot]
    -- ^ An array of intent slots. At runtime, Amazon Lex elicits required slot values from the user using prompts defined in the slots. For more information, see 'how-it-works' . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutIntent' value with any optional fields omitted.
mkPutIntent
    :: Types.IntentName -- ^ 'name'
    -> PutIntent
mkPutIntent name
  = PutIntent'{name, checksum = Core.Nothing,
               conclusionStatement = Core.Nothing,
               confirmationPrompt = Core.Nothing, createVersion = Core.Nothing,
               description = Core.Nothing, dialogCodeHook = Core.Nothing,
               followUpPrompt = Core.Nothing, fulfillmentActivity = Core.Nothing,
               inputContexts = Core.Nothing, kendraConfiguration = Core.Nothing,
               outputContexts = Core.Nothing,
               parentIntentSignature = Core.Nothing,
               rejectionStatement = Core.Nothing, sampleUtterances = Core.Nothing,
               slots = Core.Nothing}

-- | The name of the intent. The name is /not/ case sensitive. 
--
-- The name can't match a built-in intent name, or a built-in intent name with "AMAZON." removed. For example, because there is a built-in intent called @AMAZON.HelpIntent@ , you can't create a custom intent called @HelpIntent@ .
-- For a list of built-in intents, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piName :: Lens.Lens' PutIntent Types.IntentName
piName = Lens.field @"name"
{-# INLINEABLE piName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Identifies a specific revision of the @> LATEST@ version.
--
-- When you create a new intent, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception.
-- When you want to update a intent, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piChecksum :: Lens.Lens' PutIntent (Core.Maybe Core.Text)
piChecksum = Lens.field @"checksum"
{-# INLINEABLE piChecksum #-}
{-# DEPRECATED checksum "Use generic-lens or generic-optics with 'checksum' instead"  #-}

-- | The statement that you want Amazon Lex to convey to the user after the intent is successfully fulfilled by the Lambda function. 
--
-- This element is relevant only if you provide a Lambda function in the @fulfillmentActivity@ . If you return the intent to the client application, you can't specify this element.
--
-- /Note:/ Consider using 'conclusionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piConclusionStatement :: Lens.Lens' PutIntent (Core.Maybe Types.Statement)
piConclusionStatement = Lens.field @"conclusionStatement"
{-# INLINEABLE piConclusionStatement #-}
{-# DEPRECATED conclusionStatement "Use generic-lens or generic-optics with 'conclusionStatement' instead"  #-}

-- | Prompts the user to confirm the intent. This question should have a yes or no answer.
--
-- Amazon Lex uses this prompt to ensure that the user acknowledges that the intent is ready for fulfillment. For example, with the @OrderPizza@ intent, you might want to confirm that the order is correct before placing it. For other intents, such as intents that simply respond to user questions, you might not need to ask the user for confirmation before providing the information. 
--
-- /Note:/ Consider using 'confirmationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piConfirmationPrompt :: Lens.Lens' PutIntent (Core.Maybe Types.Prompt)
piConfirmationPrompt = Lens.field @"confirmationPrompt"
{-# INLINEABLE piConfirmationPrompt #-}
{-# DEPRECATED confirmationPrompt "Use generic-lens or generic-optics with 'confirmationPrompt' instead"  #-}

-- | When set to @true@ a new numbered version of the intent is created. This is the same as calling the @CreateIntentVersion@ operation. If you do not specify @createVersion@ , the default is @false@ .
--
-- /Note:/ Consider using 'createVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piCreateVersion :: Lens.Lens' PutIntent (Core.Maybe Core.Bool)
piCreateVersion = Lens.field @"createVersion"
{-# INLINEABLE piCreateVersion #-}
{-# DEPRECATED createVersion "Use generic-lens or generic-optics with 'createVersion' instead"  #-}

-- | A description of the intent.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piDescription :: Lens.Lens' PutIntent (Core.Maybe Types.Description)
piDescription = Lens.field @"description"
{-# INLINEABLE piDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Specifies a Lambda function to invoke for each user input. You can invoke this Lambda function to personalize user interaction. 
--
-- For example, suppose your bot determines that the user is John. Your Lambda function might retrieve John's information from a backend database and prepopulate some of the values. For example, if you find that John is gluten intolerant, you might set the corresponding intent slot, @GlutenIntolerant@ , to true. You might find John's phone number and set the corresponding session attribute. 
--
-- /Note:/ Consider using 'dialogCodeHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piDialogCodeHook :: Lens.Lens' PutIntent (Core.Maybe Types.CodeHook)
piDialogCodeHook = Lens.field @"dialogCodeHook"
{-# INLINEABLE piDialogCodeHook #-}
{-# DEPRECATED dialogCodeHook "Use generic-lens or generic-optics with 'dialogCodeHook' instead"  #-}

-- | Amazon Lex uses this prompt to solicit additional activity after fulfilling an intent. For example, after the @OrderPizza@ intent is fulfilled, you might prompt the user to order a drink.
--
-- The action that Amazon Lex takes depends on the user's response, as follows:
--
--     * If the user says "Yes" it responds with the clarification prompt that is configured for the bot.
--
--
--     * if the user says "Yes" and continues with an utterance that triggers an intent it starts a conversation for the intent.
--
--
--     * If the user says "No" it responds with the rejection statement configured for the the follow-up prompt.
--
--
--     * If it doesn't recognize the utterance it repeats the follow-up prompt again.
--
--
-- The @followUpPrompt@ field and the @conclusionStatement@ field are mutually exclusive. You can specify only one. 
--
-- /Note:/ Consider using 'followUpPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piFollowUpPrompt :: Lens.Lens' PutIntent (Core.Maybe Types.FollowUpPrompt)
piFollowUpPrompt = Lens.field @"followUpPrompt"
{-# INLINEABLE piFollowUpPrompt #-}
{-# DEPRECATED followUpPrompt "Use generic-lens or generic-optics with 'followUpPrompt' instead"  #-}

-- | Required. Describes how the intent is fulfilled. For example, after a user provides all of the information for a pizza order, @fulfillmentActivity@ defines how the bot places an order with a local pizza store. 
--
-- You might configure Amazon Lex to return all of the intent information to the client application, or direct it to invoke a Lambda function that can process the intent (for example, place an order with a pizzeria). 
--
-- /Note:/ Consider using 'fulfillmentActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piFulfillmentActivity :: Lens.Lens' PutIntent (Core.Maybe Types.FulfillmentActivity)
piFulfillmentActivity = Lens.field @"fulfillmentActivity"
{-# INLINEABLE piFulfillmentActivity #-}
{-# DEPRECATED fulfillmentActivity "Use generic-lens or generic-optics with 'fulfillmentActivity' instead"  #-}

-- | An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
--
-- /Note:/ Consider using 'inputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piInputContexts :: Lens.Lens' PutIntent (Core.Maybe [Types.InputContext])
piInputContexts = Lens.field @"inputContexts"
{-# INLINEABLE piInputContexts #-}
{-# DEPRECATED inputContexts "Use generic-lens or generic-optics with 'inputContexts' instead"  #-}

-- | Configuration information required to use the @AMAZON.KendraSearchIntent@ intent to connect to an Amazon Kendra index. For more information, see <http://docs.aws.amazon.com/lex/latest/dg/built-in-intent-kendra-search.html AMAZON.KendraSearchIntent> .
--
-- /Note:/ Consider using 'kendraConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piKendraConfiguration :: Lens.Lens' PutIntent (Core.Maybe Types.KendraConfiguration)
piKendraConfiguration = Lens.field @"kendraConfiguration"
{-# INLINEABLE piKendraConfiguration #-}
{-# DEPRECATED kendraConfiguration "Use generic-lens or generic-optics with 'kendraConfiguration' instead"  #-}

-- | An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
--
-- /Note:/ Consider using 'outputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piOutputContexts :: Lens.Lens' PutIntent (Core.Maybe [Types.OutputContext])
piOutputContexts = Lens.field @"outputContexts"
{-# INLINEABLE piOutputContexts #-}
{-# DEPRECATED outputContexts "Use generic-lens or generic-optics with 'outputContexts' instead"  #-}

-- | A unique identifier for the built-in intent to base this intent on. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
--
-- /Note:/ Consider using 'parentIntentSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piParentIntentSignature :: Lens.Lens' PutIntent (Core.Maybe Types.ParentIntentSignature)
piParentIntentSignature = Lens.field @"parentIntentSignature"
{-# INLINEABLE piParentIntentSignature #-}
{-# DEPRECATED parentIntentSignature "Use generic-lens or generic-optics with 'parentIntentSignature' instead"  #-}

-- | When the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled. 
--
-- /Note:/ Consider using 'rejectionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piRejectionStatement :: Lens.Lens' PutIntent (Core.Maybe Types.Statement)
piRejectionStatement = Lens.field @"rejectionStatement"
{-# INLINEABLE piRejectionStatement #-}
{-# DEPRECATED rejectionStatement "Use generic-lens or generic-optics with 'rejectionStatement' instead"  #-}

-- | An array of utterances (strings) that a user might say to signal the intent. For example, "I want {PizzaSize} pizza", "Order {Quantity} {PizzaSize} pizzas". 
--
-- In each utterance, a slot name is enclosed in curly braces. 
--
-- /Note:/ Consider using 'sampleUtterances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piSampleUtterances :: Lens.Lens' PutIntent (Core.Maybe [Types.Utterance])
piSampleUtterances = Lens.field @"sampleUtterances"
{-# INLINEABLE piSampleUtterances #-}
{-# DEPRECATED sampleUtterances "Use generic-lens or generic-optics with 'sampleUtterances' instead"  #-}

-- | An array of intent slots. At runtime, Amazon Lex elicits required slot values from the user using prompts defined in the slots. For more information, see 'how-it-works' . 
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piSlots :: Lens.Lens' PutIntent (Core.Maybe [Types.Slot])
piSlots = Lens.field @"slots"
{-# INLINEABLE piSlots #-}
{-# DEPRECATED slots "Use generic-lens or generic-optics with 'slots' instead"  #-}

instance Core.ToQuery PutIntent where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutIntent where
        toHeaders PutIntent{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutIntent where
        toJSON PutIntent{..}
          = Core.object
              (Core.catMaybes
                 [("checksum" Core..=) Core.<$> checksum,
                  ("conclusionStatement" Core..=) Core.<$> conclusionStatement,
                  ("confirmationPrompt" Core..=) Core.<$> confirmationPrompt,
                  ("createVersion" Core..=) Core.<$> createVersion,
                  ("description" Core..=) Core.<$> description,
                  ("dialogCodeHook" Core..=) Core.<$> dialogCodeHook,
                  ("followUpPrompt" Core..=) Core.<$> followUpPrompt,
                  ("fulfillmentActivity" Core..=) Core.<$> fulfillmentActivity,
                  ("inputContexts" Core..=) Core.<$> inputContexts,
                  ("kendraConfiguration" Core..=) Core.<$> kendraConfiguration,
                  ("outputContexts" Core..=) Core.<$> outputContexts,
                  ("parentIntentSignature" Core..=) Core.<$> parentIntentSignature,
                  ("rejectionStatement" Core..=) Core.<$> rejectionStatement,
                  ("sampleUtterances" Core..=) Core.<$> sampleUtterances,
                  ("slots" Core..=) Core.<$> slots])

instance Core.AWSRequest PutIntent where
        type Rs PutIntent = PutIntentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/intents/" Core.<> Core.toText name Core.<> "/versions/$LATEST",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutIntentResponse' Core.<$>
                   (x Core..:? "checksum") Core.<*> x Core..:? "conclusionStatement"
                     Core.<*> x Core..:? "confirmationPrompt"
                     Core.<*> x Core..:? "createVersion"
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

-- | /See:/ 'mkPutIntentResponse' smart constructor.
data PutIntentResponse = PutIntentResponse'
  { checksum :: Core.Maybe Core.Text
    -- ^ Checksum of the @> LATEST@ version of the intent created or updated.
  , conclusionStatement :: Core.Maybe Types.Statement
    -- ^ After the Lambda function specified in the@fulfillmentActivity@ intent fulfills the intent, Amazon Lex conveys this statement to the user.
  , confirmationPrompt :: Core.Maybe Types.Prompt
    -- ^ If defined in the intent, Amazon Lex prompts the user to confirm the intent before fulfilling it.
  , createVersion :: Core.Maybe Core.Bool
    -- ^ @True@ if a new version of the intent was created. If the @createVersion@ field was not specified in the request, the @createVersion@ field is set to false in the response.
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the intent was created.
  , description :: Core.Maybe Types.Description
    -- ^ A description of the intent.
  , dialogCodeHook :: Core.Maybe Types.CodeHook
    -- ^ If defined in the intent, Amazon Lex invokes this Lambda function for each user input.
  , followUpPrompt :: Core.Maybe Types.FollowUpPrompt
    -- ^ If defined in the intent, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled.
  , fulfillmentActivity :: Core.Maybe Types.FulfillmentActivity
    -- ^ If defined in the intent, Amazon Lex invokes this Lambda function to fulfill the intent after the user provides all of the information required by the intent.
  , inputContexts :: Core.Maybe [Types.InputContext]
    -- ^ An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
  , kendraConfiguration :: Core.Maybe Types.KendraConfiguration
    -- ^ Configuration information, if any, required to connect to an Amazon Kendra index and use the @AMAZON.KendraSearchIntent@ intent.
  , lastUpdatedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the intent was updated. When you create a resource, the creation date and last update dates are the same.
  , name :: Core.Maybe Types.IntentName
    -- ^ The name of the intent.
  , outputContexts :: Core.Maybe [Types.OutputContext]
    -- ^ An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
  , parentIntentSignature :: Core.Maybe Types.BuiltinIntentSignature
    -- ^ A unique identifier for the built-in intent that this intent is based on.
  , rejectionStatement :: Core.Maybe Types.Statement
    -- ^ If the user answers "no" to the question defined in @confirmationPrompt@ Amazon Lex responds with this statement to acknowledge that the intent was canceled. 
  , sampleUtterances :: Core.Maybe [Types.Utterance]
    -- ^ An array of sample utterances that are configured for the intent. 
  , slots :: Core.Maybe [Types.Slot]
    -- ^ An array of intent slots that are configured for the intent.
  , version :: Core.Maybe Types.Version
    -- ^ The version of the intent. For a new intent, the version is always @> LATEST@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PutIntentResponse' value with any optional fields omitted.
mkPutIntentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutIntentResponse
mkPutIntentResponse responseStatus
  = PutIntentResponse'{checksum = Core.Nothing,
                       conclusionStatement = Core.Nothing,
                       confirmationPrompt = Core.Nothing, createVersion = Core.Nothing,
                       createdDate = Core.Nothing, description = Core.Nothing,
                       dialogCodeHook = Core.Nothing, followUpPrompt = Core.Nothing,
                       fulfillmentActivity = Core.Nothing, inputContexts = Core.Nothing,
                       kendraConfiguration = Core.Nothing, lastUpdatedDate = Core.Nothing,
                       name = Core.Nothing, outputContexts = Core.Nothing,
                       parentIntentSignature = Core.Nothing,
                       rejectionStatement = Core.Nothing, sampleUtterances = Core.Nothing,
                       slots = Core.Nothing, version = Core.Nothing, responseStatus}

-- | Checksum of the @> LATEST@ version of the intent created or updated.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsChecksum :: Lens.Lens' PutIntentResponse (Core.Maybe Core.Text)
pirrsChecksum = Lens.field @"checksum"
{-# INLINEABLE pirrsChecksum #-}
{-# DEPRECATED checksum "Use generic-lens or generic-optics with 'checksum' instead"  #-}

-- | After the Lambda function specified in the@fulfillmentActivity@ intent fulfills the intent, Amazon Lex conveys this statement to the user.
--
-- /Note:/ Consider using 'conclusionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsConclusionStatement :: Lens.Lens' PutIntentResponse (Core.Maybe Types.Statement)
pirrsConclusionStatement = Lens.field @"conclusionStatement"
{-# INLINEABLE pirrsConclusionStatement #-}
{-# DEPRECATED conclusionStatement "Use generic-lens or generic-optics with 'conclusionStatement' instead"  #-}

-- | If defined in the intent, Amazon Lex prompts the user to confirm the intent before fulfilling it.
--
-- /Note:/ Consider using 'confirmationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsConfirmationPrompt :: Lens.Lens' PutIntentResponse (Core.Maybe Types.Prompt)
pirrsConfirmationPrompt = Lens.field @"confirmationPrompt"
{-# INLINEABLE pirrsConfirmationPrompt #-}
{-# DEPRECATED confirmationPrompt "Use generic-lens or generic-optics with 'confirmationPrompt' instead"  #-}

-- | @True@ if a new version of the intent was created. If the @createVersion@ field was not specified in the request, the @createVersion@ field is set to false in the response.
--
-- /Note:/ Consider using 'createVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsCreateVersion :: Lens.Lens' PutIntentResponse (Core.Maybe Core.Bool)
pirrsCreateVersion = Lens.field @"createVersion"
{-# INLINEABLE pirrsCreateVersion #-}
{-# DEPRECATED createVersion "Use generic-lens or generic-optics with 'createVersion' instead"  #-}

-- | The date that the intent was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsCreatedDate :: Lens.Lens' PutIntentResponse (Core.Maybe Core.NominalDiffTime)
pirrsCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE pirrsCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | A description of the intent.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsDescription :: Lens.Lens' PutIntentResponse (Core.Maybe Types.Description)
pirrsDescription = Lens.field @"description"
{-# INLINEABLE pirrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | If defined in the intent, Amazon Lex invokes this Lambda function for each user input.
--
-- /Note:/ Consider using 'dialogCodeHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsDialogCodeHook :: Lens.Lens' PutIntentResponse (Core.Maybe Types.CodeHook)
pirrsDialogCodeHook = Lens.field @"dialogCodeHook"
{-# INLINEABLE pirrsDialogCodeHook #-}
{-# DEPRECATED dialogCodeHook "Use generic-lens or generic-optics with 'dialogCodeHook' instead"  #-}

-- | If defined in the intent, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled.
--
-- /Note:/ Consider using 'followUpPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsFollowUpPrompt :: Lens.Lens' PutIntentResponse (Core.Maybe Types.FollowUpPrompt)
pirrsFollowUpPrompt = Lens.field @"followUpPrompt"
{-# INLINEABLE pirrsFollowUpPrompt #-}
{-# DEPRECATED followUpPrompt "Use generic-lens or generic-optics with 'followUpPrompt' instead"  #-}

-- | If defined in the intent, Amazon Lex invokes this Lambda function to fulfill the intent after the user provides all of the information required by the intent.
--
-- /Note:/ Consider using 'fulfillmentActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsFulfillmentActivity :: Lens.Lens' PutIntentResponse (Core.Maybe Types.FulfillmentActivity)
pirrsFulfillmentActivity = Lens.field @"fulfillmentActivity"
{-# INLINEABLE pirrsFulfillmentActivity #-}
{-# DEPRECATED fulfillmentActivity "Use generic-lens or generic-optics with 'fulfillmentActivity' instead"  #-}

-- | An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
--
-- /Note:/ Consider using 'inputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsInputContexts :: Lens.Lens' PutIntentResponse (Core.Maybe [Types.InputContext])
pirrsInputContexts = Lens.field @"inputContexts"
{-# INLINEABLE pirrsInputContexts #-}
{-# DEPRECATED inputContexts "Use generic-lens or generic-optics with 'inputContexts' instead"  #-}

-- | Configuration information, if any, required to connect to an Amazon Kendra index and use the @AMAZON.KendraSearchIntent@ intent.
--
-- /Note:/ Consider using 'kendraConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsKendraConfiguration :: Lens.Lens' PutIntentResponse (Core.Maybe Types.KendraConfiguration)
pirrsKendraConfiguration = Lens.field @"kendraConfiguration"
{-# INLINEABLE pirrsKendraConfiguration #-}
{-# DEPRECATED kendraConfiguration "Use generic-lens or generic-optics with 'kendraConfiguration' instead"  #-}

-- | The date that the intent was updated. When you create a resource, the creation date and last update dates are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsLastUpdatedDate :: Lens.Lens' PutIntentResponse (Core.Maybe Core.NominalDiffTime)
pirrsLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# INLINEABLE pirrsLastUpdatedDate #-}
{-# DEPRECATED lastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead"  #-}

-- | The name of the intent.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsName :: Lens.Lens' PutIntentResponse (Core.Maybe Types.IntentName)
pirrsName = Lens.field @"name"
{-# INLINEABLE pirrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
--
-- /Note:/ Consider using 'outputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsOutputContexts :: Lens.Lens' PutIntentResponse (Core.Maybe [Types.OutputContext])
pirrsOutputContexts = Lens.field @"outputContexts"
{-# INLINEABLE pirrsOutputContexts #-}
{-# DEPRECATED outputContexts "Use generic-lens or generic-optics with 'outputContexts' instead"  #-}

-- | A unique identifier for the built-in intent that this intent is based on.
--
-- /Note:/ Consider using 'parentIntentSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsParentIntentSignature :: Lens.Lens' PutIntentResponse (Core.Maybe Types.BuiltinIntentSignature)
pirrsParentIntentSignature = Lens.field @"parentIntentSignature"
{-# INLINEABLE pirrsParentIntentSignature #-}
{-# DEPRECATED parentIntentSignature "Use generic-lens or generic-optics with 'parentIntentSignature' instead"  #-}

-- | If the user answers "no" to the question defined in @confirmationPrompt@ Amazon Lex responds with this statement to acknowledge that the intent was canceled. 
--
-- /Note:/ Consider using 'rejectionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsRejectionStatement :: Lens.Lens' PutIntentResponse (Core.Maybe Types.Statement)
pirrsRejectionStatement = Lens.field @"rejectionStatement"
{-# INLINEABLE pirrsRejectionStatement #-}
{-# DEPRECATED rejectionStatement "Use generic-lens or generic-optics with 'rejectionStatement' instead"  #-}

-- | An array of sample utterances that are configured for the intent. 
--
-- /Note:/ Consider using 'sampleUtterances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsSampleUtterances :: Lens.Lens' PutIntentResponse (Core.Maybe [Types.Utterance])
pirrsSampleUtterances = Lens.field @"sampleUtterances"
{-# INLINEABLE pirrsSampleUtterances #-}
{-# DEPRECATED sampleUtterances "Use generic-lens or generic-optics with 'sampleUtterances' instead"  #-}

-- | An array of intent slots that are configured for the intent.
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsSlots :: Lens.Lens' PutIntentResponse (Core.Maybe [Types.Slot])
pirrsSlots = Lens.field @"slots"
{-# INLINEABLE pirrsSlots #-}
{-# DEPRECATED slots "Use generic-lens or generic-optics with 'slots' instead"  #-}

-- | The version of the intent. For a new intent, the version is always @> LATEST@ .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsVersion :: Lens.Lens' PutIntentResponse (Core.Maybe Types.Version)
pirrsVersion = Lens.field @"version"
{-# INLINEABLE pirrsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsResponseStatus :: Lens.Lens' PutIntentResponse Core.Int
pirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
