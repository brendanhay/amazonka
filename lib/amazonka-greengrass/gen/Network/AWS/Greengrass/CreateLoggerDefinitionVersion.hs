{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateLoggerDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a logger definition that has already been defined.
module Network.AWS.Greengrass.CreateLoggerDefinitionVersion
  ( -- * Creating a request
    CreateLoggerDefinitionVersion (..),
    mkCreateLoggerDefinitionVersion,

    -- ** Request lenses
    cldvLoggerDefinitionId,
    cldvLoggers,
    cldvAmznClientToken,

    -- * Destructuring the response
    CreateLoggerDefinitionVersionResponse (..),
    mkCreateLoggerDefinitionVersionResponse,

    -- ** Response lenses
    cldvrsARN,
    cldvrsCreationTimestamp,
    cldvrsVersion,
    cldvrsId,
    cldvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateLoggerDefinitionVersion' smart constructor.
data CreateLoggerDefinitionVersion = CreateLoggerDefinitionVersion'
  { -- | The ID of the logger definition.
    loggerDefinitionId :: Lude.Text,
    -- | A list of loggers.
    loggers :: Lude.Maybe [GreengrassLogger],
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLoggerDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'loggerDefinitionId' - The ID of the logger definition.
-- * 'loggers' - A list of loggers.
-- * 'amznClientToken' - A client token used to correlate requests and responses.
mkCreateLoggerDefinitionVersion ::
  -- | 'loggerDefinitionId'
  Lude.Text ->
  CreateLoggerDefinitionVersion
mkCreateLoggerDefinitionVersion pLoggerDefinitionId_ =
  CreateLoggerDefinitionVersion'
    { loggerDefinitionId =
        pLoggerDefinitionId_,
      loggers = Lude.Nothing,
      amznClientToken = Lude.Nothing
    }

-- | The ID of the logger definition.
--
-- /Note:/ Consider using 'loggerDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldvLoggerDefinitionId :: Lens.Lens' CreateLoggerDefinitionVersion Lude.Text
cldvLoggerDefinitionId = Lens.lens (loggerDefinitionId :: CreateLoggerDefinitionVersion -> Lude.Text) (\s a -> s {loggerDefinitionId = a} :: CreateLoggerDefinitionVersion)
{-# DEPRECATED cldvLoggerDefinitionId "Use generic-lens or generic-optics with 'loggerDefinitionId' instead." #-}

-- | A list of loggers.
--
-- /Note:/ Consider using 'loggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldvLoggers :: Lens.Lens' CreateLoggerDefinitionVersion (Lude.Maybe [GreengrassLogger])
cldvLoggers = Lens.lens (loggers :: CreateLoggerDefinitionVersion -> Lude.Maybe [GreengrassLogger]) (\s a -> s {loggers = a} :: CreateLoggerDefinitionVersion)
{-# DEPRECATED cldvLoggers "Use generic-lens or generic-optics with 'loggers' instead." #-}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldvAmznClientToken :: Lens.Lens' CreateLoggerDefinitionVersion (Lude.Maybe Lude.Text)
cldvAmznClientToken = Lens.lens (amznClientToken :: CreateLoggerDefinitionVersion -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateLoggerDefinitionVersion)
{-# DEPRECATED cldvAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

instance Lude.AWSRequest CreateLoggerDefinitionVersion where
  type
    Rs CreateLoggerDefinitionVersion =
      CreateLoggerDefinitionVersionResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateLoggerDefinitionVersionResponse'
            Lude.<$> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Version")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateLoggerDefinitionVersion where
  toHeaders CreateLoggerDefinitionVersion' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateLoggerDefinitionVersion where
  toJSON CreateLoggerDefinitionVersion' {..} =
    Lude.object
      (Lude.catMaybes [("Loggers" Lude..=) Lude.<$> loggers])

instance Lude.ToPath CreateLoggerDefinitionVersion where
  toPath CreateLoggerDefinitionVersion' {..} =
    Lude.mconcat
      [ "/greengrass/definition/loggers/",
        Lude.toBS loggerDefinitionId,
        "/versions"
      ]

instance Lude.ToQuery CreateLoggerDefinitionVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateLoggerDefinitionVersionResponse' smart constructor.
data CreateLoggerDefinitionVersionResponse = CreateLoggerDefinitionVersionResponse'
  { -- | The ARN of the version.
    arn :: Lude.Maybe Lude.Text,
    -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Lude.Maybe Lude.Text,
    -- | The ID of the version.
    version :: Lude.Maybe Lude.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLoggerDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the version.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
-- * 'version' - The ID of the version.
-- * 'id' - The ID of the parent definition that the version is associated with.
-- * 'responseStatus' - The response status code.
mkCreateLoggerDefinitionVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateLoggerDefinitionVersionResponse
mkCreateLoggerDefinitionVersionResponse pResponseStatus_ =
  CreateLoggerDefinitionVersionResponse'
    { arn = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldvrsARN :: Lens.Lens' CreateLoggerDefinitionVersionResponse (Lude.Maybe Lude.Text)
cldvrsARN = Lens.lens (arn :: CreateLoggerDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateLoggerDefinitionVersionResponse)
{-# DEPRECATED cldvrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldvrsCreationTimestamp :: Lens.Lens' CreateLoggerDefinitionVersionResponse (Lude.Maybe Lude.Text)
cldvrsCreationTimestamp = Lens.lens (creationTimestamp :: CreateLoggerDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: CreateLoggerDefinitionVersionResponse)
{-# DEPRECATED cldvrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldvrsVersion :: Lens.Lens' CreateLoggerDefinitionVersionResponse (Lude.Maybe Lude.Text)
cldvrsVersion = Lens.lens (version :: CreateLoggerDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: CreateLoggerDefinitionVersionResponse)
{-# DEPRECATED cldvrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The ID of the parent definition that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldvrsId :: Lens.Lens' CreateLoggerDefinitionVersionResponse (Lude.Maybe Lude.Text)
cldvrsId = Lens.lens (id :: CreateLoggerDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateLoggerDefinitionVersionResponse)
{-# DEPRECATED cldvrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldvrsResponseStatus :: Lens.Lens' CreateLoggerDefinitionVersionResponse Lude.Int
cldvrsResponseStatus = Lens.lens (responseStatus :: CreateLoggerDefinitionVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLoggerDefinitionVersionResponse)
{-# DEPRECATED cldvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
