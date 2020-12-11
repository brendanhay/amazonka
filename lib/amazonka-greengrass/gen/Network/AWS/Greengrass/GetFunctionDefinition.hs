{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetFunctionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a Lambda function definition, including its creation time and latest version.
module Network.AWS.Greengrass.GetFunctionDefinition
  ( -- * Creating a request
    GetFunctionDefinition (..),
    mkGetFunctionDefinition,

    -- ** Request lenses
    gfdFunctionDefinitionId,

    -- * Destructuring the response
    GetFunctionDefinitionResponse (..),
    mkGetFunctionDefinitionResponse,

    -- ** Response lenses
    gfdrsLatestVersionARN,
    gfdrsARN,
    gfdrsName,
    gfdrsCreationTimestamp,
    gfdrsId,
    gfdrsLatestVersion,
    gfdrsLastUpdatedTimestamp,
    gfdrsTags,
    gfdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFunctionDefinition' smart constructor.
newtype GetFunctionDefinition = GetFunctionDefinition'
  { functionDefinitionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFunctionDefinition' with the minimum fields required to make a request.
--
-- * 'functionDefinitionId' - The ID of the Lambda function definition.
mkGetFunctionDefinition ::
  -- | 'functionDefinitionId'
  Lude.Text ->
  GetFunctionDefinition
mkGetFunctionDefinition pFunctionDefinitionId_ =
  GetFunctionDefinition'
    { functionDefinitionId =
        pFunctionDefinitionId_
    }

-- | The ID of the Lambda function definition.
--
-- /Note:/ Consider using 'functionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdFunctionDefinitionId :: Lens.Lens' GetFunctionDefinition Lude.Text
gfdFunctionDefinitionId = Lens.lens (functionDefinitionId :: GetFunctionDefinition -> Lude.Text) (\s a -> s {functionDefinitionId = a} :: GetFunctionDefinition)
{-# DEPRECATED gfdFunctionDefinitionId "Use generic-lens or generic-optics with 'functionDefinitionId' instead." #-}

instance Lude.AWSRequest GetFunctionDefinition where
  type Rs GetFunctionDefinition = GetFunctionDefinitionResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFunctionDefinitionResponse'
            Lude.<$> (x Lude..?> "LatestVersionArn")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (x Lude..?> "LatestVersion")
            Lude.<*> (x Lude..?> "LastUpdatedTimestamp")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFunctionDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetFunctionDefinition where
  toPath GetFunctionDefinition' {..} =
    Lude.mconcat
      [ "/greengrass/definition/functions/",
        Lude.toBS functionDefinitionId
      ]

instance Lude.ToQuery GetFunctionDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetFunctionDefinitionResponse' smart constructor.
data GetFunctionDefinitionResponse = GetFunctionDefinitionResponse'
  { latestVersionARN ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    creationTimestamp ::
      Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    latestVersion ::
      Lude.Maybe Lude.Text,
    lastUpdatedTimestamp ::
      Lude.Maybe Lude.Text,
    tags ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
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

-- | Creates a value of 'GetFunctionDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the definition.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
-- * 'id' - The ID of the definition.
-- * 'lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
-- * 'latestVersion' - The ID of the latest version associated with the definition.
-- * 'latestVersionARN' - The ARN of the latest version associated with the definition.
-- * 'name' - The name of the definition.
-- * 'responseStatus' - The response status code.
-- * 'tags' - Tag(s) attached to the resource arn.
mkGetFunctionDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetFunctionDefinitionResponse
mkGetFunctionDefinitionResponse pResponseStatus_ =
  GetFunctionDefinitionResponse'
    { latestVersionARN = Lude.Nothing,
      arn = Lude.Nothing,
      name = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      id = Lude.Nothing,
      latestVersion = Lude.Nothing,
      lastUpdatedTimestamp = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrsLatestVersionARN :: Lens.Lens' GetFunctionDefinitionResponse (Lude.Maybe Lude.Text)
gfdrsLatestVersionARN = Lens.lens (latestVersionARN :: GetFunctionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersionARN = a} :: GetFunctionDefinitionResponse)
{-# DEPRECATED gfdrsLatestVersionARN "Use generic-lens or generic-optics with 'latestVersionARN' instead." #-}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrsARN :: Lens.Lens' GetFunctionDefinitionResponse (Lude.Maybe Lude.Text)
gfdrsARN = Lens.lens (arn :: GetFunctionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetFunctionDefinitionResponse)
{-# DEPRECATED gfdrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrsName :: Lens.Lens' GetFunctionDefinitionResponse (Lude.Maybe Lude.Text)
gfdrsName = Lens.lens (name :: GetFunctionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetFunctionDefinitionResponse)
{-# DEPRECATED gfdrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrsCreationTimestamp :: Lens.Lens' GetFunctionDefinitionResponse (Lude.Maybe Lude.Text)
gfdrsCreationTimestamp = Lens.lens (creationTimestamp :: GetFunctionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: GetFunctionDefinitionResponse)
{-# DEPRECATED gfdrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrsId :: Lens.Lens' GetFunctionDefinitionResponse (Lude.Maybe Lude.Text)
gfdrsId = Lens.lens (id :: GetFunctionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GetFunctionDefinitionResponse)
{-# DEPRECATED gfdrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrsLatestVersion :: Lens.Lens' GetFunctionDefinitionResponse (Lude.Maybe Lude.Text)
gfdrsLatestVersion = Lens.lens (latestVersion :: GetFunctionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: GetFunctionDefinitionResponse)
{-# DEPRECATED gfdrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrsLastUpdatedTimestamp :: Lens.Lens' GetFunctionDefinitionResponse (Lude.Maybe Lude.Text)
gfdrsLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: GetFunctionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedTimestamp = a} :: GetFunctionDefinitionResponse)
{-# DEPRECATED gfdrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrsTags :: Lens.Lens' GetFunctionDefinitionResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gfdrsTags = Lens.lens (tags :: GetFunctionDefinitionResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: GetFunctionDefinitionResponse)
{-# DEPRECATED gfdrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrsResponseStatus :: Lens.Lens' GetFunctionDefinitionResponse Lude.Int
gfdrsResponseStatus = Lens.lens (responseStatus :: GetFunctionDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFunctionDefinitionResponse)
{-# DEPRECATED gfdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
