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
    civChecksum,
    civName,

    -- * Destructuring the response
    CreateIntentVersionResponse (..),
    mkCreateIntentVersionResponse,

    -- ** Response lenses
    civrsFulfillmentActivity,
    civrsSlots,
    civrsRejectionStatement,
    civrsChecksum,
    civrsConclusionStatement,
    civrsSampleUtterances,
    civrsParentIntentSignature,
    civrsCreatedDate,
    civrsKendraConfiguration,
    civrsName,
    civrsVersion,
    civrsInputContexts,
    civrsFollowUpPrompt,
    civrsLastUpdatedDate,
    civrsOutputContexts,
    civrsConfirmationPrompt,
    civrsDialogCodeHook,
    civrsDescription,
    civrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateIntentVersion' smart constructor.
data CreateIntentVersion = CreateIntentVersion'
  { -- | Checksum of the @> LATEST@ version of the intent that should be used to create the new version. If you specify a checksum and the @> LATEST@ version of the intent has a different checksum, Amazon Lex returns a @PreconditionFailedException@ exception and doesn't publish a new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
    checksum :: Lude.Maybe Lude.Text,
    -- | The name of the intent that you want to create a new version of. The name is case sensitive.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateIntentVersion' with the minimum fields required to make a request.
--
-- * 'checksum' - Checksum of the @> LATEST@ version of the intent that should be used to create the new version. If you specify a checksum and the @> LATEST@ version of the intent has a different checksum, Amazon Lex returns a @PreconditionFailedException@ exception and doesn't publish a new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
-- * 'name' - The name of the intent that you want to create a new version of. The name is case sensitive.
mkCreateIntentVersion ::
  -- | 'name'
  Lude.Text ->
  CreateIntentVersion
mkCreateIntentVersion pName_ =
  CreateIntentVersion' {checksum = Lude.Nothing, name = pName_}

-- | Checksum of the @> LATEST@ version of the intent that should be used to create the new version. If you specify a checksum and the @> LATEST@ version of the intent has a different checksum, Amazon Lex returns a @PreconditionFailedException@ exception and doesn't publish a new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civChecksum :: Lens.Lens' CreateIntentVersion (Lude.Maybe Lude.Text)
civChecksum = Lens.lens (checksum :: CreateIntentVersion -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: CreateIntentVersion)
{-# DEPRECATED civChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | The name of the intent that you want to create a new version of. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civName :: Lens.Lens' CreateIntentVersion Lude.Text
civName = Lens.lens (name :: CreateIntentVersion -> Lude.Text) (\s a -> s {name = a} :: CreateIntentVersion)
{-# DEPRECATED civName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateIntentVersion where
  type Rs CreateIntentVersion = CreateIntentVersionResponse
  request = Req.postJSON lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateIntentVersionResponse'
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

instance Lude.ToHeaders CreateIntentVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateIntentVersion where
  toJSON CreateIntentVersion' {..} =
    Lude.object
      (Lude.catMaybes [("checksum" Lude..=) Lude.<$> checksum])

instance Lude.ToPath CreateIntentVersion where
  toPath CreateIntentVersion' {..} =
    Lude.mconcat ["/intents/", Lude.toBS name, "/versions"]

instance Lude.ToQuery CreateIntentVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateIntentVersionResponse' smart constructor.
data CreateIntentVersionResponse = CreateIntentVersionResponse'
  { -- | Describes how the intent is fulfilled.
    fulfillmentActivity :: Lude.Maybe FulfillmentActivity,
    -- | An array of slot types that defines the information required to fulfill the intent.
    slots :: Lude.Maybe [Slot],
    -- | If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
    rejectionStatement :: Lude.Maybe Statement,
    -- | Checksum of the intent version created.
    checksum :: Lude.Maybe Lude.Text,
    -- | After the Lambda function specified in the @fulfillmentActivity@ field fulfills the intent, Amazon Lex conveys this statement to the user.
    conclusionStatement :: Lude.Maybe Statement,
    -- | An array of sample utterances configured for the intent.
    sampleUtterances :: Lude.Maybe [Lude.Text],
    -- | A unique identifier for a built-in intent.
    parentIntentSignature :: Lude.Maybe Lude.Text,
    -- | The date that the intent was created.
    createdDate :: Lude.Maybe Lude.Timestamp,
    -- | Configuration information, if any, for connecting an Amazon Kendra index with the @AMAZON.KendraSearchIntent@ intent.
    kendraConfiguration :: Lude.Maybe KendraConfiguration,
    -- | The name of the intent.
    name :: Lude.Maybe Lude.Text,
    -- | The version number assigned to the new version of the intent.
    version :: Lude.Maybe Lude.Text,
    -- | An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
    inputContexts :: Lude.Maybe [InputContext],
    -- | If defined, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled.
    followUpPrompt :: Lude.Maybe FollowUpPrompt,
    -- | The date that the intent was updated.
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp,
    -- | An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
    outputContexts :: Lude.Maybe [OutputContext],
    -- | If defined, the prompt that Amazon Lex uses to confirm the user's intent before fulfilling it.
    confirmationPrompt :: Lude.Maybe Prompt,
    -- | If defined, Amazon Lex invokes this Lambda function for each user input.
    dialogCodeHook :: Lude.Maybe CodeHook,
    -- | A description of the intent.
    description :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateIntentVersionResponse' with the minimum fields required to make a request.
--
-- * 'fulfillmentActivity' - Describes how the intent is fulfilled.
-- * 'slots' - An array of slot types that defines the information required to fulfill the intent.
-- * 'rejectionStatement' - If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
-- * 'checksum' - Checksum of the intent version created.
-- * 'conclusionStatement' - After the Lambda function specified in the @fulfillmentActivity@ field fulfills the intent, Amazon Lex conveys this statement to the user.
-- * 'sampleUtterances' - An array of sample utterances configured for the intent.
-- * 'parentIntentSignature' - A unique identifier for a built-in intent.
-- * 'createdDate' - The date that the intent was created.
-- * 'kendraConfiguration' - Configuration information, if any, for connecting an Amazon Kendra index with the @AMAZON.KendraSearchIntent@ intent.
-- * 'name' - The name of the intent.
-- * 'version' - The version number assigned to the new version of the intent.
-- * 'inputContexts' - An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
-- * 'followUpPrompt' - If defined, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled.
-- * 'lastUpdatedDate' - The date that the intent was updated.
-- * 'outputContexts' - An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
-- * 'confirmationPrompt' - If defined, the prompt that Amazon Lex uses to confirm the user's intent before fulfilling it.
-- * 'dialogCodeHook' - If defined, Amazon Lex invokes this Lambda function for each user input.
-- * 'description' - A description of the intent.
-- * 'responseStatus' - The response status code.
mkCreateIntentVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateIntentVersionResponse
mkCreateIntentVersionResponse pResponseStatus_ =
  CreateIntentVersionResponse'
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

-- | Describes how the intent is fulfilled.
--
-- /Note:/ Consider using 'fulfillmentActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsFulfillmentActivity :: Lens.Lens' CreateIntentVersionResponse (Lude.Maybe FulfillmentActivity)
civrsFulfillmentActivity = Lens.lens (fulfillmentActivity :: CreateIntentVersionResponse -> Lude.Maybe FulfillmentActivity) (\s a -> s {fulfillmentActivity = a} :: CreateIntentVersionResponse)
{-# DEPRECATED civrsFulfillmentActivity "Use generic-lens or generic-optics with 'fulfillmentActivity' instead." #-}

-- | An array of slot types that defines the information required to fulfill the intent.
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsSlots :: Lens.Lens' CreateIntentVersionResponse (Lude.Maybe [Slot])
civrsSlots = Lens.lens (slots :: CreateIntentVersionResponse -> Lude.Maybe [Slot]) (\s a -> s {slots = a} :: CreateIntentVersionResponse)
{-# DEPRECATED civrsSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
--
-- /Note:/ Consider using 'rejectionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsRejectionStatement :: Lens.Lens' CreateIntentVersionResponse (Lude.Maybe Statement)
civrsRejectionStatement = Lens.lens (rejectionStatement :: CreateIntentVersionResponse -> Lude.Maybe Statement) (\s a -> s {rejectionStatement = a} :: CreateIntentVersionResponse)
{-# DEPRECATED civrsRejectionStatement "Use generic-lens or generic-optics with 'rejectionStatement' instead." #-}

-- | Checksum of the intent version created.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsChecksum :: Lens.Lens' CreateIntentVersionResponse (Lude.Maybe Lude.Text)
civrsChecksum = Lens.lens (checksum :: CreateIntentVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: CreateIntentVersionResponse)
{-# DEPRECATED civrsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | After the Lambda function specified in the @fulfillmentActivity@ field fulfills the intent, Amazon Lex conveys this statement to the user.
--
-- /Note:/ Consider using 'conclusionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsConclusionStatement :: Lens.Lens' CreateIntentVersionResponse (Lude.Maybe Statement)
civrsConclusionStatement = Lens.lens (conclusionStatement :: CreateIntentVersionResponse -> Lude.Maybe Statement) (\s a -> s {conclusionStatement = a} :: CreateIntentVersionResponse)
{-# DEPRECATED civrsConclusionStatement "Use generic-lens or generic-optics with 'conclusionStatement' instead." #-}

-- | An array of sample utterances configured for the intent.
--
-- /Note:/ Consider using 'sampleUtterances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsSampleUtterances :: Lens.Lens' CreateIntentVersionResponse (Lude.Maybe [Lude.Text])
civrsSampleUtterances = Lens.lens (sampleUtterances :: CreateIntentVersionResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {sampleUtterances = a} :: CreateIntentVersionResponse)
{-# DEPRECATED civrsSampleUtterances "Use generic-lens or generic-optics with 'sampleUtterances' instead." #-}

-- | A unique identifier for a built-in intent.
--
-- /Note:/ Consider using 'parentIntentSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsParentIntentSignature :: Lens.Lens' CreateIntentVersionResponse (Lude.Maybe Lude.Text)
civrsParentIntentSignature = Lens.lens (parentIntentSignature :: CreateIntentVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {parentIntentSignature = a} :: CreateIntentVersionResponse)
{-# DEPRECATED civrsParentIntentSignature "Use generic-lens or generic-optics with 'parentIntentSignature' instead." #-}

-- | The date that the intent was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsCreatedDate :: Lens.Lens' CreateIntentVersionResponse (Lude.Maybe Lude.Timestamp)
civrsCreatedDate = Lens.lens (createdDate :: CreateIntentVersionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: CreateIntentVersionResponse)
{-# DEPRECATED civrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | Configuration information, if any, for connecting an Amazon Kendra index with the @AMAZON.KendraSearchIntent@ intent.
--
-- /Note:/ Consider using 'kendraConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsKendraConfiguration :: Lens.Lens' CreateIntentVersionResponse (Lude.Maybe KendraConfiguration)
civrsKendraConfiguration = Lens.lens (kendraConfiguration :: CreateIntentVersionResponse -> Lude.Maybe KendraConfiguration) (\s a -> s {kendraConfiguration = a} :: CreateIntentVersionResponse)
{-# DEPRECATED civrsKendraConfiguration "Use generic-lens or generic-optics with 'kendraConfiguration' instead." #-}

-- | The name of the intent.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsName :: Lens.Lens' CreateIntentVersionResponse (Lude.Maybe Lude.Text)
civrsName = Lens.lens (name :: CreateIntentVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateIntentVersionResponse)
{-# DEPRECATED civrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version number assigned to the new version of the intent.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsVersion :: Lens.Lens' CreateIntentVersionResponse (Lude.Maybe Lude.Text)
civrsVersion = Lens.lens (version :: CreateIntentVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: CreateIntentVersionResponse)
{-# DEPRECATED civrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
--
-- /Note:/ Consider using 'inputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsInputContexts :: Lens.Lens' CreateIntentVersionResponse (Lude.Maybe [InputContext])
civrsInputContexts = Lens.lens (inputContexts :: CreateIntentVersionResponse -> Lude.Maybe [InputContext]) (\s a -> s {inputContexts = a} :: CreateIntentVersionResponse)
{-# DEPRECATED civrsInputContexts "Use generic-lens or generic-optics with 'inputContexts' instead." #-}

-- | If defined, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled.
--
-- /Note:/ Consider using 'followUpPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsFollowUpPrompt :: Lens.Lens' CreateIntentVersionResponse (Lude.Maybe FollowUpPrompt)
civrsFollowUpPrompt = Lens.lens (followUpPrompt :: CreateIntentVersionResponse -> Lude.Maybe FollowUpPrompt) (\s a -> s {followUpPrompt = a} :: CreateIntentVersionResponse)
{-# DEPRECATED civrsFollowUpPrompt "Use generic-lens or generic-optics with 'followUpPrompt' instead." #-}

-- | The date that the intent was updated.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsLastUpdatedDate :: Lens.Lens' CreateIntentVersionResponse (Lude.Maybe Lude.Timestamp)
civrsLastUpdatedDate = Lens.lens (lastUpdatedDate :: CreateIntentVersionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: CreateIntentVersionResponse)
{-# DEPRECATED civrsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
--
-- /Note:/ Consider using 'outputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsOutputContexts :: Lens.Lens' CreateIntentVersionResponse (Lude.Maybe [OutputContext])
civrsOutputContexts = Lens.lens (outputContexts :: CreateIntentVersionResponse -> Lude.Maybe [OutputContext]) (\s a -> s {outputContexts = a} :: CreateIntentVersionResponse)
{-# DEPRECATED civrsOutputContexts "Use generic-lens or generic-optics with 'outputContexts' instead." #-}

-- | If defined, the prompt that Amazon Lex uses to confirm the user's intent before fulfilling it.
--
-- /Note:/ Consider using 'confirmationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsConfirmationPrompt :: Lens.Lens' CreateIntentVersionResponse (Lude.Maybe Prompt)
civrsConfirmationPrompt = Lens.lens (confirmationPrompt :: CreateIntentVersionResponse -> Lude.Maybe Prompt) (\s a -> s {confirmationPrompt = a} :: CreateIntentVersionResponse)
{-# DEPRECATED civrsConfirmationPrompt "Use generic-lens or generic-optics with 'confirmationPrompt' instead." #-}

-- | If defined, Amazon Lex invokes this Lambda function for each user input.
--
-- /Note:/ Consider using 'dialogCodeHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsDialogCodeHook :: Lens.Lens' CreateIntentVersionResponse (Lude.Maybe CodeHook)
civrsDialogCodeHook = Lens.lens (dialogCodeHook :: CreateIntentVersionResponse -> Lude.Maybe CodeHook) (\s a -> s {dialogCodeHook = a} :: CreateIntentVersionResponse)
{-# DEPRECATED civrsDialogCodeHook "Use generic-lens or generic-optics with 'dialogCodeHook' instead." #-}

-- | A description of the intent.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsDescription :: Lens.Lens' CreateIntentVersionResponse (Lude.Maybe Lude.Text)
civrsDescription = Lens.lens (description :: CreateIntentVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateIntentVersionResponse)
{-# DEPRECATED civrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsResponseStatus :: Lens.Lens' CreateIntentVersionResponse Lude.Int
civrsResponseStatus = Lens.lens (responseStatus :: CreateIntentVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateIntentVersionResponse)
{-# DEPRECATED civrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
