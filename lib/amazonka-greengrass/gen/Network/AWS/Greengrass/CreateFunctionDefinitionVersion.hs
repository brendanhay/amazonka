{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateFunctionDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a Lambda function definition that has already been defined.
module Network.AWS.Greengrass.CreateFunctionDefinitionVersion
  ( -- * Creating a request
    CreateFunctionDefinitionVersion (..),
    mkCreateFunctionDefinitionVersion,

    -- ** Request lenses
    cfdvAmznClientToken,
    cfdvDefaultConfig,
    cfdvFunctions,
    cfdvFunctionDefinitionId,

    -- * Destructuring the response
    CreateFunctionDefinitionVersionResponse (..),
    mkCreateFunctionDefinitionVersionResponse,

    -- ** Response lenses
    cfdvrsARN,
    cfdvrsCreationTimestamp,
    cfdvrsVersion,
    cfdvrsId,
    cfdvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Information needed to create a function definition version.
--
-- /See:/ 'mkCreateFunctionDefinitionVersion' smart constructor.
data CreateFunctionDefinitionVersion = CreateFunctionDefinitionVersion'
  { amznClientToken ::
      Lude.Maybe Lude.Text,
    defaultConfig ::
      Lude.Maybe
        FunctionDefaultConfig,
    functions ::
      Lude.Maybe [Function],
    functionDefinitionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFunctionDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 'defaultConfig' - The default configuration that applies to all Lambda functions in this function definition version. Individual Lambda functions can override these settings.
-- * 'functionDefinitionId' - The ID of the Lambda function definition.
-- * 'functions' - A list of Lambda functions in this function definition version.
mkCreateFunctionDefinitionVersion ::
  -- | 'functionDefinitionId'
  Lude.Text ->
  CreateFunctionDefinitionVersion
mkCreateFunctionDefinitionVersion pFunctionDefinitionId_ =
  CreateFunctionDefinitionVersion'
    { amznClientToken = Lude.Nothing,
      defaultConfig = Lude.Nothing,
      functions = Lude.Nothing,
      functionDefinitionId = pFunctionDefinitionId_
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdvAmznClientToken :: Lens.Lens' CreateFunctionDefinitionVersion (Lude.Maybe Lude.Text)
cfdvAmznClientToken = Lens.lens (amznClientToken :: CreateFunctionDefinitionVersion -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateFunctionDefinitionVersion)
{-# DEPRECATED cfdvAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | The default configuration that applies to all Lambda functions in this function definition version. Individual Lambda functions can override these settings.
--
-- /Note:/ Consider using 'defaultConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdvDefaultConfig :: Lens.Lens' CreateFunctionDefinitionVersion (Lude.Maybe FunctionDefaultConfig)
cfdvDefaultConfig = Lens.lens (defaultConfig :: CreateFunctionDefinitionVersion -> Lude.Maybe FunctionDefaultConfig) (\s a -> s {defaultConfig = a} :: CreateFunctionDefinitionVersion)
{-# DEPRECATED cfdvDefaultConfig "Use generic-lens or generic-optics with 'defaultConfig' instead." #-}

-- | A list of Lambda functions in this function definition version.
--
-- /Note:/ Consider using 'functions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdvFunctions :: Lens.Lens' CreateFunctionDefinitionVersion (Lude.Maybe [Function])
cfdvFunctions = Lens.lens (functions :: CreateFunctionDefinitionVersion -> Lude.Maybe [Function]) (\s a -> s {functions = a} :: CreateFunctionDefinitionVersion)
{-# DEPRECATED cfdvFunctions "Use generic-lens or generic-optics with 'functions' instead." #-}

-- | The ID of the Lambda function definition.
--
-- /Note:/ Consider using 'functionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdvFunctionDefinitionId :: Lens.Lens' CreateFunctionDefinitionVersion Lude.Text
cfdvFunctionDefinitionId = Lens.lens (functionDefinitionId :: CreateFunctionDefinitionVersion -> Lude.Text) (\s a -> s {functionDefinitionId = a} :: CreateFunctionDefinitionVersion)
{-# DEPRECATED cfdvFunctionDefinitionId "Use generic-lens or generic-optics with 'functionDefinitionId' instead." #-}

instance Lude.AWSRequest CreateFunctionDefinitionVersion where
  type
    Rs CreateFunctionDefinitionVersion =
      CreateFunctionDefinitionVersionResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateFunctionDefinitionVersionResponse'
            Lude.<$> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Version")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateFunctionDefinitionVersion where
  toHeaders CreateFunctionDefinitionVersion' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateFunctionDefinitionVersion where
  toJSON CreateFunctionDefinitionVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DefaultConfig" Lude..=) Lude.<$> defaultConfig,
            ("Functions" Lude..=) Lude.<$> functions
          ]
      )

instance Lude.ToPath CreateFunctionDefinitionVersion where
  toPath CreateFunctionDefinitionVersion' {..} =
    Lude.mconcat
      [ "/greengrass/definition/functions/",
        Lude.toBS functionDefinitionId,
        "/versions"
      ]

instance Lude.ToQuery CreateFunctionDefinitionVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateFunctionDefinitionVersionResponse' smart constructor.
data CreateFunctionDefinitionVersionResponse = CreateFunctionDefinitionVersionResponse'
  { arn ::
      Lude.Maybe
        Lude.Text,
    creationTimestamp ::
      Lude.Maybe
        Lude.Text,
    version ::
      Lude.Maybe
        Lude.Text,
    id ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFunctionDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the version.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
-- * 'id' - The ID of the parent definition that the version is associated with.
-- * 'responseStatus' - The response status code.
-- * 'version' - The ID of the version.
mkCreateFunctionDefinitionVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateFunctionDefinitionVersionResponse
mkCreateFunctionDefinitionVersionResponse pResponseStatus_ =
  CreateFunctionDefinitionVersionResponse'
    { arn = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdvrsARN :: Lens.Lens' CreateFunctionDefinitionVersionResponse (Lude.Maybe Lude.Text)
cfdvrsARN = Lens.lens (arn :: CreateFunctionDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateFunctionDefinitionVersionResponse)
{-# DEPRECATED cfdvrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdvrsCreationTimestamp :: Lens.Lens' CreateFunctionDefinitionVersionResponse (Lude.Maybe Lude.Text)
cfdvrsCreationTimestamp = Lens.lens (creationTimestamp :: CreateFunctionDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: CreateFunctionDefinitionVersionResponse)
{-# DEPRECATED cfdvrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdvrsVersion :: Lens.Lens' CreateFunctionDefinitionVersionResponse (Lude.Maybe Lude.Text)
cfdvrsVersion = Lens.lens (version :: CreateFunctionDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: CreateFunctionDefinitionVersionResponse)
{-# DEPRECATED cfdvrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The ID of the parent definition that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdvrsId :: Lens.Lens' CreateFunctionDefinitionVersionResponse (Lude.Maybe Lude.Text)
cfdvrsId = Lens.lens (id :: CreateFunctionDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateFunctionDefinitionVersionResponse)
{-# DEPRECATED cfdvrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdvrsResponseStatus :: Lens.Lens' CreateFunctionDefinitionVersionResponse Lude.Int
cfdvrsResponseStatus = Lens.lens (responseStatus :: CreateFunctionDefinitionVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateFunctionDefinitionVersionResponse)
{-# DEPRECATED cfdvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
