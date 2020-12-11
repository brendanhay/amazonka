{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    getrsFulfillmentActivity,
    getrsSlots,
    getrsRejectionStatement,
    getrsChecksum,
    getrsConclusionStatement,
    getrsSampleUtterances,
    getrsParentIntentSignature,
    getrsCreatedDate,
    getrsKendraConfiguration,
    getrsName,
    getrsVersion,
    getrsInputContexts,
    getrsFollowUpPrompt,
    getrsLastUpdatedDate,
    getrsOutputContexts,
    getrsConfirmationPrompt,
    getrsDialogCodeHook,
    getrsDescription,
    getrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetIntent' smart constructor.
data GetIntent = GetIntent'
  { name :: Lude.Text,
    version :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
  { fulfillmentActivity ::
      Lude.Maybe FulfillmentActivity,
    slots :: Lude.Maybe [Slot],
    rejectionStatement :: Lude.Maybe Statement,
    checksum :: Lude.Maybe Lude.Text,
    conclusionStatement :: Lude.Maybe Statement,
    sampleUtterances :: Lude.Maybe [Lude.Text],
    parentIntentSignature :: Lude.Maybe Lude.Text,
    createdDate :: Lude.Maybe Lude.Timestamp,
    kendraConfiguration :: Lude.Maybe KendraConfiguration,
    name :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text,
    inputContexts :: Lude.Maybe [InputContext],
    followUpPrompt :: Lude.Maybe FollowUpPrompt,
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp,
    outputContexts :: Lude.Maybe [OutputContext],
    confirmationPrompt :: Lude.Maybe Prompt,
    dialogCodeHook :: Lude.Maybe CodeHook,
    description :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIntentResponse' with the minimum fields required to make a request.
--
-- * 'checksum' - Checksum of the intent.
-- * 'conclusionStatement' - After the Lambda function specified in the @fulfillmentActivity@ element fulfills the intent, Amazon Lex conveys this statement to the user.
-- * 'confirmationPrompt' - If defined in the bot, Amazon Lex uses prompt to confirm the intent before fulfilling the user's request. For more information, see 'PutIntent' .
-- * 'createdDate' - The date that the intent was created.
-- * 'description' - A description of the intent.
-- * 'dialogCodeHook' - If defined in the bot, Amazon Amazon Lex invokes this Lambda function for each user input. For more information, see 'PutIntent' .
-- * 'followUpPrompt' - If defined in the bot, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled. For more information, see 'PutIntent' .
-- * 'fulfillmentActivity' - Describes how the intent is fulfilled. For more information, see 'PutIntent' .
-- * 'inputContexts' - An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
-- * 'kendraConfiguration' - Configuration information, if any, to connect to an Amazon Kendra index with the @AMAZON.KendraSearchIntent@ intent.
-- * 'lastUpdatedDate' - The date that the intent was updated. When you create a resource, the creation date and the last updated date are the same.
-- * 'name' - The name of the intent.
-- * 'outputContexts' - An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
-- * 'parentIntentSignature' - A unique identifier for a built-in intent.
-- * 'rejectionStatement' - If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
-- * 'responseStatus' - The response status code.
-- * 'sampleUtterances' - An array of sample utterances configured for the intent.
-- * 'slots' - An array of intent slots configured for the intent.
-- * 'version' - The version of the intent.
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
getrsFulfillmentActivity :: Lens.Lens' GetIntentResponse (Lude.Maybe FulfillmentActivity)
getrsFulfillmentActivity = Lens.lens (fulfillmentActivity :: GetIntentResponse -> Lude.Maybe FulfillmentActivity) (\s a -> s {fulfillmentActivity = a} :: GetIntentResponse)
{-# DEPRECATED getrsFulfillmentActivity "Use generic-lens or generic-optics with 'fulfillmentActivity' instead." #-}

-- | An array of intent slots configured for the intent.
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsSlots :: Lens.Lens' GetIntentResponse (Lude.Maybe [Slot])
getrsSlots = Lens.lens (slots :: GetIntentResponse -> Lude.Maybe [Slot]) (\s a -> s {slots = a} :: GetIntentResponse)
{-# DEPRECATED getrsSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
--
-- /Note:/ Consider using 'rejectionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsRejectionStatement :: Lens.Lens' GetIntentResponse (Lude.Maybe Statement)
getrsRejectionStatement = Lens.lens (rejectionStatement :: GetIntentResponse -> Lude.Maybe Statement) (\s a -> s {rejectionStatement = a} :: GetIntentResponse)
{-# DEPRECATED getrsRejectionStatement "Use generic-lens or generic-optics with 'rejectionStatement' instead." #-}

-- | Checksum of the intent.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsChecksum :: Lens.Lens' GetIntentResponse (Lude.Maybe Lude.Text)
getrsChecksum = Lens.lens (checksum :: GetIntentResponse -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: GetIntentResponse)
{-# DEPRECATED getrsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | After the Lambda function specified in the @fulfillmentActivity@ element fulfills the intent, Amazon Lex conveys this statement to the user.
--
-- /Note:/ Consider using 'conclusionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsConclusionStatement :: Lens.Lens' GetIntentResponse (Lude.Maybe Statement)
getrsConclusionStatement = Lens.lens (conclusionStatement :: GetIntentResponse -> Lude.Maybe Statement) (\s a -> s {conclusionStatement = a} :: GetIntentResponse)
{-# DEPRECATED getrsConclusionStatement "Use generic-lens or generic-optics with 'conclusionStatement' instead." #-}

-- | An array of sample utterances configured for the intent.
--
-- /Note:/ Consider using 'sampleUtterances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsSampleUtterances :: Lens.Lens' GetIntentResponse (Lude.Maybe [Lude.Text])
getrsSampleUtterances = Lens.lens (sampleUtterances :: GetIntentResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {sampleUtterances = a} :: GetIntentResponse)
{-# DEPRECATED getrsSampleUtterances "Use generic-lens or generic-optics with 'sampleUtterances' instead." #-}

-- | A unique identifier for a built-in intent.
--
-- /Note:/ Consider using 'parentIntentSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsParentIntentSignature :: Lens.Lens' GetIntentResponse (Lude.Maybe Lude.Text)
getrsParentIntentSignature = Lens.lens (parentIntentSignature :: GetIntentResponse -> Lude.Maybe Lude.Text) (\s a -> s {parentIntentSignature = a} :: GetIntentResponse)
{-# DEPRECATED getrsParentIntentSignature "Use generic-lens or generic-optics with 'parentIntentSignature' instead." #-}

-- | The date that the intent was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsCreatedDate :: Lens.Lens' GetIntentResponse (Lude.Maybe Lude.Timestamp)
getrsCreatedDate = Lens.lens (createdDate :: GetIntentResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: GetIntentResponse)
{-# DEPRECATED getrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | Configuration information, if any, to connect to an Amazon Kendra index with the @AMAZON.KendraSearchIntent@ intent.
--
-- /Note:/ Consider using 'kendraConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsKendraConfiguration :: Lens.Lens' GetIntentResponse (Lude.Maybe KendraConfiguration)
getrsKendraConfiguration = Lens.lens (kendraConfiguration :: GetIntentResponse -> Lude.Maybe KendraConfiguration) (\s a -> s {kendraConfiguration = a} :: GetIntentResponse)
{-# DEPRECATED getrsKendraConfiguration "Use generic-lens or generic-optics with 'kendraConfiguration' instead." #-}

-- | The name of the intent.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsName :: Lens.Lens' GetIntentResponse (Lude.Maybe Lude.Text)
getrsName = Lens.lens (name :: GetIntentResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetIntentResponse)
{-# DEPRECATED getrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the intent.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsVersion :: Lens.Lens' GetIntentResponse (Lude.Maybe Lude.Text)
getrsVersion = Lens.lens (version :: GetIntentResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: GetIntentResponse)
{-# DEPRECATED getrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
--
-- /Note:/ Consider using 'inputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsInputContexts :: Lens.Lens' GetIntentResponse (Lude.Maybe [InputContext])
getrsInputContexts = Lens.lens (inputContexts :: GetIntentResponse -> Lude.Maybe [InputContext]) (\s a -> s {inputContexts = a} :: GetIntentResponse)
{-# DEPRECATED getrsInputContexts "Use generic-lens or generic-optics with 'inputContexts' instead." #-}

-- | If defined in the bot, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled. For more information, see 'PutIntent' .
--
-- /Note:/ Consider using 'followUpPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsFollowUpPrompt :: Lens.Lens' GetIntentResponse (Lude.Maybe FollowUpPrompt)
getrsFollowUpPrompt = Lens.lens (followUpPrompt :: GetIntentResponse -> Lude.Maybe FollowUpPrompt) (\s a -> s {followUpPrompt = a} :: GetIntentResponse)
{-# DEPRECATED getrsFollowUpPrompt "Use generic-lens or generic-optics with 'followUpPrompt' instead." #-}

-- | The date that the intent was updated. When you create a resource, the creation date and the last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsLastUpdatedDate :: Lens.Lens' GetIntentResponse (Lude.Maybe Lude.Timestamp)
getrsLastUpdatedDate = Lens.lens (lastUpdatedDate :: GetIntentResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: GetIntentResponse)
{-# DEPRECATED getrsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
--
-- /Note:/ Consider using 'outputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsOutputContexts :: Lens.Lens' GetIntentResponse (Lude.Maybe [OutputContext])
getrsOutputContexts = Lens.lens (outputContexts :: GetIntentResponse -> Lude.Maybe [OutputContext]) (\s a -> s {outputContexts = a} :: GetIntentResponse)
{-# DEPRECATED getrsOutputContexts "Use generic-lens or generic-optics with 'outputContexts' instead." #-}

-- | If defined in the bot, Amazon Lex uses prompt to confirm the intent before fulfilling the user's request. For more information, see 'PutIntent' .
--
-- /Note:/ Consider using 'confirmationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsConfirmationPrompt :: Lens.Lens' GetIntentResponse (Lude.Maybe Prompt)
getrsConfirmationPrompt = Lens.lens (confirmationPrompt :: GetIntentResponse -> Lude.Maybe Prompt) (\s a -> s {confirmationPrompt = a} :: GetIntentResponse)
{-# DEPRECATED getrsConfirmationPrompt "Use generic-lens or generic-optics with 'confirmationPrompt' instead." #-}

-- | If defined in the bot, Amazon Amazon Lex invokes this Lambda function for each user input. For more information, see 'PutIntent' .
--
-- /Note:/ Consider using 'dialogCodeHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsDialogCodeHook :: Lens.Lens' GetIntentResponse (Lude.Maybe CodeHook)
getrsDialogCodeHook = Lens.lens (dialogCodeHook :: GetIntentResponse -> Lude.Maybe CodeHook) (\s a -> s {dialogCodeHook = a} :: GetIntentResponse)
{-# DEPRECATED getrsDialogCodeHook "Use generic-lens or generic-optics with 'dialogCodeHook' instead." #-}

-- | A description of the intent.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsDescription :: Lens.Lens' GetIntentResponse (Lude.Maybe Lude.Text)
getrsDescription = Lens.lens (description :: GetIntentResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GetIntentResponse)
{-# DEPRECATED getrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsResponseStatus :: Lens.Lens' GetIntentResponse Lude.Int
getrsResponseStatus = Lens.lens (responseStatus :: GetIntentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetIntentResponse)
{-# DEPRECATED getrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
