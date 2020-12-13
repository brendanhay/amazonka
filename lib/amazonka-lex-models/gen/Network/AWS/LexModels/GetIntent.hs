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
    gifrsFulfillmentActivity,
    gifrsSlots,
    gifrsRejectionStatement,
    gifrsChecksum,
    gifrsConclusionStatement,
    gifrsSampleUtterances,
    gifrsParentIntentSignature,
    gifrsCreatedDate,
    gifrsKendraConfiguration,
    gifrsName,
    gifrsVersion,
    gifrsInputContexts,
    gifrsFollowUpPrompt,
    gifrsLastUpdatedDate,
    gifrsOutputContexts,
    gifrsConfirmationPrompt,
    gifrsDialogCodeHook,
    gifrsDescription,
    gifrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetIntent' smart constructor.
data GetIntent = GetIntent'
  { -- | The name of the intent. The name is case sensitive.
    name :: Lude.Text,
    -- | The version of the intent.
    version :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIntent' with the minimum fields required to make a request.
--
-- * 'name' - The name of the intent. The name is case sensitive.
-- * 'version' - The version of the intent.
mkGetIntent ::
  -- | 'name'
  Lude.Text ->
  -- | 'version'
  Lude.Text ->
  GetIntent
mkGetIntent pName_ pVersion_ =
  GetIntent' {name = pName_, version = pVersion_}

-- | The name of the intent. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giName :: Lens.Lens' GetIntent Lude.Text
giName = Lens.lens (name :: GetIntent -> Lude.Text) (\s a -> s {name = a} :: GetIntent)
{-# DEPRECATED giName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the intent.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giVersion :: Lens.Lens' GetIntent Lude.Text
giVersion = Lens.lens (version :: GetIntent -> Lude.Text) (\s a -> s {version = a} :: GetIntent)
{-# DEPRECATED giVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.AWSRequest GetIntent where
  type Rs GetIntent = GetIntentResponse
  request = Req.get lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetIntentResponse'
            Lude.<$> (x Lude..?> "fulfillmentActivity")
            Lude.<*> (x Lude..?> "slots" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "rejectionStatement")
            Lude.<*> (x Lude..?> "checksum")
            Lude.<*> (x Lude..?> "conclusionStatement")
            Lude.<*> (x Lude..?> "sampleUtterances" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "parentIntentSignature")
            Lude.<*> (x Lude..?> "createdDate")
            Lude.<*> (x Lude..?> "kendraConfiguration")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "version")
            Lude.<*> (x Lude..?> "inputContexts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "followUpPrompt")
            Lude.<*> (x Lude..?> "lastUpdatedDate")
            Lude.<*> (x Lude..?> "outputContexts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "confirmationPrompt")
            Lude.<*> (x Lude..?> "dialogCodeHook")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetIntent where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetIntent where
  toPath GetIntent' {..} =
    Lude.mconcat
      ["/intents/", Lude.toBS name, "/versions/", Lude.toBS version]

instance Lude.ToQuery GetIntent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetIntentResponse' smart constructor.
data GetIntentResponse = GetIntentResponse'
  { -- | Describes how the intent is fulfilled. For more information, see 'PutIntent' .
    fulfillmentActivity :: Lude.Maybe FulfillmentActivity,
    -- | An array of intent slots configured for the intent.
    slots :: Lude.Maybe [Slot],
    -- | If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
    rejectionStatement :: Lude.Maybe Statement,
    -- | Checksum of the intent.
    checksum :: Lude.Maybe Lude.Text,
    -- | After the Lambda function specified in the @fulfillmentActivity@ element fulfills the intent, Amazon Lex conveys this statement to the user.
    conclusionStatement :: Lude.Maybe Statement,
    -- | An array of sample utterances configured for the intent.
    sampleUtterances :: Lude.Maybe [Lude.Text],
    -- | A unique identifier for a built-in intent.
    parentIntentSignature :: Lude.Maybe Lude.Text,
    -- | The date that the intent was created.
    createdDate :: Lude.Maybe Lude.Timestamp,
    -- | Configuration information, if any, to connect to an Amazon Kendra index with the @AMAZON.KendraSearchIntent@ intent.
    kendraConfiguration :: Lude.Maybe KendraConfiguration,
    -- | The name of the intent.
    name :: Lude.Maybe Lude.Text,
    -- | The version of the intent.
    version :: Lude.Maybe Lude.Text,
    -- | An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
    inputContexts :: Lude.Maybe [InputContext],
    -- | If defined in the bot, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled. For more information, see 'PutIntent' .
    followUpPrompt :: Lude.Maybe FollowUpPrompt,
    -- | The date that the intent was updated. When you create a resource, the creation date and the last updated date are the same.
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp,
    -- | An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
    outputContexts :: Lude.Maybe [OutputContext],
    -- | If defined in the bot, Amazon Lex uses prompt to confirm the intent before fulfilling the user's request. For more information, see 'PutIntent' .
    confirmationPrompt :: Lude.Maybe Prompt,
    -- | If defined in the bot, Amazon Amazon Lex invokes this Lambda function for each user input. For more information, see 'PutIntent' .
    dialogCodeHook :: Lude.Maybe CodeHook,
    -- | A description of the intent.
    description :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIntentResponse' with the minimum fields required to make a request.
--
-- * 'fulfillmentActivity' - Describes how the intent is fulfilled. For more information, see 'PutIntent' .
-- * 'slots' - An array of intent slots configured for the intent.
-- * 'rejectionStatement' - If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
-- * 'checksum' - Checksum of the intent.
-- * 'conclusionStatement' - After the Lambda function specified in the @fulfillmentActivity@ element fulfills the intent, Amazon Lex conveys this statement to the user.
-- * 'sampleUtterances' - An array of sample utterances configured for the intent.
-- * 'parentIntentSignature' - A unique identifier for a built-in intent.
-- * 'createdDate' - The date that the intent was created.
-- * 'kendraConfiguration' - Configuration information, if any, to connect to an Amazon Kendra index with the @AMAZON.KendraSearchIntent@ intent.
-- * 'name' - The name of the intent.
-- * 'version' - The version of the intent.
-- * 'inputContexts' - An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
-- * 'followUpPrompt' - If defined in the bot, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled. For more information, see 'PutIntent' .
-- * 'lastUpdatedDate' - The date that the intent was updated. When you create a resource, the creation date and the last updated date are the same.
-- * 'outputContexts' - An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
-- * 'confirmationPrompt' - If defined in the bot, Amazon Lex uses prompt to confirm the intent before fulfilling the user's request. For more information, see 'PutIntent' .
-- * 'dialogCodeHook' - If defined in the bot, Amazon Amazon Lex invokes this Lambda function for each user input. For more information, see 'PutIntent' .
-- * 'description' - A description of the intent.
-- * 'responseStatus' - The response status code.
mkGetIntentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetIntentResponse
mkGetIntentResponse pResponseStatus_ =
  GetIntentResponse'
    { fulfillmentActivity = Lude.Nothing,
      slots = Lude.Nothing,
      rejectionStatement = Lude.Nothing,
      checksum = Lude.Nothing,
      conclusionStatement = Lude.Nothing,
      sampleUtterances = Lude.Nothing,
      parentIntentSignature = Lude.Nothing,
      createdDate = Lude.Nothing,
      kendraConfiguration = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing,
      inputContexts = Lude.Nothing,
      followUpPrompt = Lude.Nothing,
      lastUpdatedDate = Lude.Nothing,
      outputContexts = Lude.Nothing,
      confirmationPrompt = Lude.Nothing,
      dialogCodeHook = Lude.Nothing,
      description = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Describes how the intent is fulfilled. For more information, see 'PutIntent' .
--
-- /Note:/ Consider using 'fulfillmentActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gifrsFulfillmentActivity :: Lens.Lens' GetIntentResponse (Lude.Maybe FulfillmentActivity)
gifrsFulfillmentActivity = Lens.lens (fulfillmentActivity :: GetIntentResponse -> Lude.Maybe FulfillmentActivity) (\s a -> s {fulfillmentActivity = a} :: GetIntentResponse)
{-# DEPRECATED gifrsFulfillmentActivity "Use generic-lens or generic-optics with 'fulfillmentActivity' instead." #-}

-- | An array of intent slots configured for the intent.
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gifrsSlots :: Lens.Lens' GetIntentResponse (Lude.Maybe [Slot])
gifrsSlots = Lens.lens (slots :: GetIntentResponse -> Lude.Maybe [Slot]) (\s a -> s {slots = a} :: GetIntentResponse)
{-# DEPRECATED gifrsSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
--
-- /Note:/ Consider using 'rejectionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gifrsRejectionStatement :: Lens.Lens' GetIntentResponse (Lude.Maybe Statement)
gifrsRejectionStatement = Lens.lens (rejectionStatement :: GetIntentResponse -> Lude.Maybe Statement) (\s a -> s {rejectionStatement = a} :: GetIntentResponse)
{-# DEPRECATED gifrsRejectionStatement "Use generic-lens or generic-optics with 'rejectionStatement' instead." #-}

-- | Checksum of the intent.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gifrsChecksum :: Lens.Lens' GetIntentResponse (Lude.Maybe Lude.Text)
gifrsChecksum = Lens.lens (checksum :: GetIntentResponse -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: GetIntentResponse)
{-# DEPRECATED gifrsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | After the Lambda function specified in the @fulfillmentActivity@ element fulfills the intent, Amazon Lex conveys this statement to the user.
--
-- /Note:/ Consider using 'conclusionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gifrsConclusionStatement :: Lens.Lens' GetIntentResponse (Lude.Maybe Statement)
gifrsConclusionStatement = Lens.lens (conclusionStatement :: GetIntentResponse -> Lude.Maybe Statement) (\s a -> s {conclusionStatement = a} :: GetIntentResponse)
{-# DEPRECATED gifrsConclusionStatement "Use generic-lens or generic-optics with 'conclusionStatement' instead." #-}

-- | An array of sample utterances configured for the intent.
--
-- /Note:/ Consider using 'sampleUtterances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gifrsSampleUtterances :: Lens.Lens' GetIntentResponse (Lude.Maybe [Lude.Text])
gifrsSampleUtterances = Lens.lens (sampleUtterances :: GetIntentResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {sampleUtterances = a} :: GetIntentResponse)
{-# DEPRECATED gifrsSampleUtterances "Use generic-lens or generic-optics with 'sampleUtterances' instead." #-}

-- | A unique identifier for a built-in intent.
--
-- /Note:/ Consider using 'parentIntentSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gifrsParentIntentSignature :: Lens.Lens' GetIntentResponse (Lude.Maybe Lude.Text)
gifrsParentIntentSignature = Lens.lens (parentIntentSignature :: GetIntentResponse -> Lude.Maybe Lude.Text) (\s a -> s {parentIntentSignature = a} :: GetIntentResponse)
{-# DEPRECATED gifrsParentIntentSignature "Use generic-lens or generic-optics with 'parentIntentSignature' instead." #-}

-- | The date that the intent was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gifrsCreatedDate :: Lens.Lens' GetIntentResponse (Lude.Maybe Lude.Timestamp)
gifrsCreatedDate = Lens.lens (createdDate :: GetIntentResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: GetIntentResponse)
{-# DEPRECATED gifrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | Configuration information, if any, to connect to an Amazon Kendra index with the @AMAZON.KendraSearchIntent@ intent.
--
-- /Note:/ Consider using 'kendraConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gifrsKendraConfiguration :: Lens.Lens' GetIntentResponse (Lude.Maybe KendraConfiguration)
gifrsKendraConfiguration = Lens.lens (kendraConfiguration :: GetIntentResponse -> Lude.Maybe KendraConfiguration) (\s a -> s {kendraConfiguration = a} :: GetIntentResponse)
{-# DEPRECATED gifrsKendraConfiguration "Use generic-lens or generic-optics with 'kendraConfiguration' instead." #-}

-- | The name of the intent.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gifrsName :: Lens.Lens' GetIntentResponse (Lude.Maybe Lude.Text)
gifrsName = Lens.lens (name :: GetIntentResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetIntentResponse)
{-# DEPRECATED gifrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the intent.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gifrsVersion :: Lens.Lens' GetIntentResponse (Lude.Maybe Lude.Text)
gifrsVersion = Lens.lens (version :: GetIntentResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: GetIntentResponse)
{-# DEPRECATED gifrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
--
-- /Note:/ Consider using 'inputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gifrsInputContexts :: Lens.Lens' GetIntentResponse (Lude.Maybe [InputContext])
gifrsInputContexts = Lens.lens (inputContexts :: GetIntentResponse -> Lude.Maybe [InputContext]) (\s a -> s {inputContexts = a} :: GetIntentResponse)
{-# DEPRECATED gifrsInputContexts "Use generic-lens or generic-optics with 'inputContexts' instead." #-}

-- | If defined in the bot, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled. For more information, see 'PutIntent' .
--
-- /Note:/ Consider using 'followUpPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gifrsFollowUpPrompt :: Lens.Lens' GetIntentResponse (Lude.Maybe FollowUpPrompt)
gifrsFollowUpPrompt = Lens.lens (followUpPrompt :: GetIntentResponse -> Lude.Maybe FollowUpPrompt) (\s a -> s {followUpPrompt = a} :: GetIntentResponse)
{-# DEPRECATED gifrsFollowUpPrompt "Use generic-lens or generic-optics with 'followUpPrompt' instead." #-}

-- | The date that the intent was updated. When you create a resource, the creation date and the last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gifrsLastUpdatedDate :: Lens.Lens' GetIntentResponse (Lude.Maybe Lude.Timestamp)
gifrsLastUpdatedDate = Lens.lens (lastUpdatedDate :: GetIntentResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: GetIntentResponse)
{-# DEPRECATED gifrsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
--
-- /Note:/ Consider using 'outputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gifrsOutputContexts :: Lens.Lens' GetIntentResponse (Lude.Maybe [OutputContext])
gifrsOutputContexts = Lens.lens (outputContexts :: GetIntentResponse -> Lude.Maybe [OutputContext]) (\s a -> s {outputContexts = a} :: GetIntentResponse)
{-# DEPRECATED gifrsOutputContexts "Use generic-lens or generic-optics with 'outputContexts' instead." #-}

-- | If defined in the bot, Amazon Lex uses prompt to confirm the intent before fulfilling the user's request. For more information, see 'PutIntent' .
--
-- /Note:/ Consider using 'confirmationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gifrsConfirmationPrompt :: Lens.Lens' GetIntentResponse (Lude.Maybe Prompt)
gifrsConfirmationPrompt = Lens.lens (confirmationPrompt :: GetIntentResponse -> Lude.Maybe Prompt) (\s a -> s {confirmationPrompt = a} :: GetIntentResponse)
{-# DEPRECATED gifrsConfirmationPrompt "Use generic-lens or generic-optics with 'confirmationPrompt' instead." #-}

-- | If defined in the bot, Amazon Amazon Lex invokes this Lambda function for each user input. For more information, see 'PutIntent' .
--
-- /Note:/ Consider using 'dialogCodeHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gifrsDialogCodeHook :: Lens.Lens' GetIntentResponse (Lude.Maybe CodeHook)
gifrsDialogCodeHook = Lens.lens (dialogCodeHook :: GetIntentResponse -> Lude.Maybe CodeHook) (\s a -> s {dialogCodeHook = a} :: GetIntentResponse)
{-# DEPRECATED gifrsDialogCodeHook "Use generic-lens or generic-optics with 'dialogCodeHook' instead." #-}

-- | A description of the intent.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gifrsDescription :: Lens.Lens' GetIntentResponse (Lude.Maybe Lude.Text)
gifrsDescription = Lens.lens (description :: GetIntentResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GetIntentResponse)
{-# DEPRECATED gifrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gifrsResponseStatus :: Lens.Lens' GetIntentResponse Lude.Int
gifrsResponseStatus = Lens.lens (responseStatus :: GetIntentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetIntentResponse)
{-# DEPRECATED gifrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
