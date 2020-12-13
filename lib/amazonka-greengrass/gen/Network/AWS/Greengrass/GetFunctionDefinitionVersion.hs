{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetFunctionDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a Lambda function definition version, including which Lambda functions are included in the version and their configurations.
module Network.AWS.Greengrass.GetFunctionDefinitionVersion
  ( -- * Creating a request
    GetFunctionDefinitionVersion (..),
    mkGetFunctionDefinitionVersion,

    -- ** Request lenses
    gfdvNextToken,
    gfdvFunctionDefinitionId,
    gfdvFunctionDefinitionVersionId,

    -- * Destructuring the response
    GetFunctionDefinitionVersionResponse (..),
    mkGetFunctionDefinitionVersionResponse,

    -- ** Response lenses
    gfdvrsDefinition,
    gfdvrsARN,
    gfdvrsNextToken,
    gfdvrsCreationTimestamp,
    gfdvrsVersion,
    gfdvrsId,
    gfdvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFunctionDefinitionVersion' smart constructor.
data GetFunctionDefinitionVersion = GetFunctionDefinitionVersion'
  { -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the Lambda function definition.
    functionDefinitionId :: Lude.Text,
    -- | The ID of the function definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListFunctionDefinitionVersions'' requests. If the version is the last one that was associated with a function definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
    functionDefinitionVersionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFunctionDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'functionDefinitionId' - The ID of the Lambda function definition.
-- * 'functionDefinitionVersionId' - The ID of the function definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListFunctionDefinitionVersions'' requests. If the version is the last one that was associated with a function definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
mkGetFunctionDefinitionVersion ::
  -- | 'functionDefinitionId'
  Lude.Text ->
  -- | 'functionDefinitionVersionId'
  Lude.Text ->
  GetFunctionDefinitionVersion
mkGetFunctionDefinitionVersion
  pFunctionDefinitionId_
  pFunctionDefinitionVersionId_ =
    GetFunctionDefinitionVersion'
      { nextToken = Lude.Nothing,
        functionDefinitionId = pFunctionDefinitionId_,
        functionDefinitionVersionId = pFunctionDefinitionVersionId_
      }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdvNextToken :: Lens.Lens' GetFunctionDefinitionVersion (Lude.Maybe Lude.Text)
gfdvNextToken = Lens.lens (nextToken :: GetFunctionDefinitionVersion -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetFunctionDefinitionVersion)
{-# DEPRECATED gfdvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the Lambda function definition.
--
-- /Note:/ Consider using 'functionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdvFunctionDefinitionId :: Lens.Lens' GetFunctionDefinitionVersion Lude.Text
gfdvFunctionDefinitionId = Lens.lens (functionDefinitionId :: GetFunctionDefinitionVersion -> Lude.Text) (\s a -> s {functionDefinitionId = a} :: GetFunctionDefinitionVersion)
{-# DEPRECATED gfdvFunctionDefinitionId "Use generic-lens or generic-optics with 'functionDefinitionId' instead." #-}

-- | The ID of the function definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListFunctionDefinitionVersions'' requests. If the version is the last one that was associated with a function definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
--
-- /Note:/ Consider using 'functionDefinitionVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdvFunctionDefinitionVersionId :: Lens.Lens' GetFunctionDefinitionVersion Lude.Text
gfdvFunctionDefinitionVersionId = Lens.lens (functionDefinitionVersionId :: GetFunctionDefinitionVersion -> Lude.Text) (\s a -> s {functionDefinitionVersionId = a} :: GetFunctionDefinitionVersion)
{-# DEPRECATED gfdvFunctionDefinitionVersionId "Use generic-lens or generic-optics with 'functionDefinitionVersionId' instead." #-}

instance Lude.AWSRequest GetFunctionDefinitionVersion where
  type
    Rs GetFunctionDefinitionVersion =
      GetFunctionDefinitionVersionResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFunctionDefinitionVersionResponse'
            Lude.<$> (x Lude..?> "Definition")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Version")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFunctionDefinitionVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetFunctionDefinitionVersion where
  toPath GetFunctionDefinitionVersion' {..} =
    Lude.mconcat
      [ "/greengrass/definition/functions/",
        Lude.toBS functionDefinitionId,
        "/versions/",
        Lude.toBS functionDefinitionVersionId
      ]

instance Lude.ToQuery GetFunctionDefinitionVersion where
  toQuery GetFunctionDefinitionVersion' {..} =
    Lude.mconcat ["NextToken" Lude.=: nextToken]

-- | /See:/ 'mkGetFunctionDefinitionVersionResponse' smart constructor.
data GetFunctionDefinitionVersionResponse = GetFunctionDefinitionVersionResponse'
  { -- | Information on the definition.
    definition :: Lude.Maybe FunctionDefinitionVersion,
    -- | The ARN of the function definition version.
    arn :: Lude.Maybe Lude.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The time, in milliseconds since the epoch, when the function definition version was created.
    creationTimestamp :: Lude.Maybe Lude.Text,
    -- | The version of the function definition version.
    version :: Lude.Maybe Lude.Text,
    -- | The ID of the function definition version.
    id :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFunctionDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- * 'definition' - Information on the definition.
-- * 'arn' - The ARN of the function definition version.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the function definition version was created.
-- * 'version' - The version of the function definition version.
-- * 'id' - The ID of the function definition version.
-- * 'responseStatus' - The response status code.
mkGetFunctionDefinitionVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetFunctionDefinitionVersionResponse
mkGetFunctionDefinitionVersionResponse pResponseStatus_ =
  GetFunctionDefinitionVersionResponse'
    { definition = Lude.Nothing,
      arn = Lude.Nothing,
      nextToken = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information on the definition.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdvrsDefinition :: Lens.Lens' GetFunctionDefinitionVersionResponse (Lude.Maybe FunctionDefinitionVersion)
gfdvrsDefinition = Lens.lens (definition :: GetFunctionDefinitionVersionResponse -> Lude.Maybe FunctionDefinitionVersion) (\s a -> s {definition = a} :: GetFunctionDefinitionVersionResponse)
{-# DEPRECATED gfdvrsDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The ARN of the function definition version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdvrsARN :: Lens.Lens' GetFunctionDefinitionVersionResponse (Lude.Maybe Lude.Text)
gfdvrsARN = Lens.lens (arn :: GetFunctionDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetFunctionDefinitionVersionResponse)
{-# DEPRECATED gfdvrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdvrsNextToken :: Lens.Lens' GetFunctionDefinitionVersionResponse (Lude.Maybe Lude.Text)
gfdvrsNextToken = Lens.lens (nextToken :: GetFunctionDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetFunctionDefinitionVersionResponse)
{-# DEPRECATED gfdvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The time, in milliseconds since the epoch, when the function definition version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdvrsCreationTimestamp :: Lens.Lens' GetFunctionDefinitionVersionResponse (Lude.Maybe Lude.Text)
gfdvrsCreationTimestamp = Lens.lens (creationTimestamp :: GetFunctionDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: GetFunctionDefinitionVersionResponse)
{-# DEPRECATED gfdvrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The version of the function definition version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdvrsVersion :: Lens.Lens' GetFunctionDefinitionVersionResponse (Lude.Maybe Lude.Text)
gfdvrsVersion = Lens.lens (version :: GetFunctionDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: GetFunctionDefinitionVersionResponse)
{-# DEPRECATED gfdvrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The ID of the function definition version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdvrsId :: Lens.Lens' GetFunctionDefinitionVersionResponse (Lude.Maybe Lude.Text)
gfdvrsId = Lens.lens (id :: GetFunctionDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GetFunctionDefinitionVersionResponse)
{-# DEPRECATED gfdvrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdvrsResponseStatus :: Lens.Lens' GetFunctionDefinitionVersionResponse Lude.Int
gfdvrsResponseStatus = Lens.lens (responseStatus :: GetFunctionDefinitionVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFunctionDefinitionVersionResponse)
{-# DEPRECATED gfdvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
