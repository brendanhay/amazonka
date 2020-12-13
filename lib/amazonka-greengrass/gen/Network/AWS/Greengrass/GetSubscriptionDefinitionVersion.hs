{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetSubscriptionDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a subscription definition version.
module Network.AWS.Greengrass.GetSubscriptionDefinitionVersion
  ( -- * Creating a request
    GetSubscriptionDefinitionVersion (..),
    mkGetSubscriptionDefinitionVersion,

    -- ** Request lenses
    gsdvSubscriptionDefinitionId,
    gsdvNextToken,
    gsdvSubscriptionDefinitionVersionId,

    -- * Destructuring the response
    GetSubscriptionDefinitionVersionResponse (..),
    mkGetSubscriptionDefinitionVersionResponse,

    -- ** Response lenses
    gsdvrsDefinition,
    gsdvrsARN,
    gsdvrsNextToken,
    gsdvrsCreationTimestamp,
    gsdvrsVersion,
    gsdvrsId,
    gsdvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSubscriptionDefinitionVersion' smart constructor.
data GetSubscriptionDefinitionVersion = GetSubscriptionDefinitionVersion'
  { -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Lude.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the subscription definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListSubscriptionDefinitionVersions'' requests. If the version is the last one that was associated with a subscription definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
    subscriptionDefinitionVersionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSubscriptionDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'subscriptionDefinitionId' - The ID of the subscription definition.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'subscriptionDefinitionVersionId' - The ID of the subscription definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListSubscriptionDefinitionVersions'' requests. If the version is the last one that was associated with a subscription definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
mkGetSubscriptionDefinitionVersion ::
  -- | 'subscriptionDefinitionId'
  Lude.Text ->
  -- | 'subscriptionDefinitionVersionId'
  Lude.Text ->
  GetSubscriptionDefinitionVersion
mkGetSubscriptionDefinitionVersion
  pSubscriptionDefinitionId_
  pSubscriptionDefinitionVersionId_ =
    GetSubscriptionDefinitionVersion'
      { subscriptionDefinitionId =
          pSubscriptionDefinitionId_,
        nextToken = Lude.Nothing,
        subscriptionDefinitionVersionId =
          pSubscriptionDefinitionVersionId_
      }

-- | The ID of the subscription definition.
--
-- /Note:/ Consider using 'subscriptionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvSubscriptionDefinitionId :: Lens.Lens' GetSubscriptionDefinitionVersion Lude.Text
gsdvSubscriptionDefinitionId = Lens.lens (subscriptionDefinitionId :: GetSubscriptionDefinitionVersion -> Lude.Text) (\s a -> s {subscriptionDefinitionId = a} :: GetSubscriptionDefinitionVersion)
{-# DEPRECATED gsdvSubscriptionDefinitionId "Use generic-lens or generic-optics with 'subscriptionDefinitionId' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvNextToken :: Lens.Lens' GetSubscriptionDefinitionVersion (Lude.Maybe Lude.Text)
gsdvNextToken = Lens.lens (nextToken :: GetSubscriptionDefinitionVersion -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetSubscriptionDefinitionVersion)
{-# DEPRECATED gsdvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the subscription definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListSubscriptionDefinitionVersions'' requests. If the version is the last one that was associated with a subscription definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
--
-- /Note:/ Consider using 'subscriptionDefinitionVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvSubscriptionDefinitionVersionId :: Lens.Lens' GetSubscriptionDefinitionVersion Lude.Text
gsdvSubscriptionDefinitionVersionId = Lens.lens (subscriptionDefinitionVersionId :: GetSubscriptionDefinitionVersion -> Lude.Text) (\s a -> s {subscriptionDefinitionVersionId = a} :: GetSubscriptionDefinitionVersion)
{-# DEPRECATED gsdvSubscriptionDefinitionVersionId "Use generic-lens or generic-optics with 'subscriptionDefinitionVersionId' instead." #-}

instance Lude.AWSRequest GetSubscriptionDefinitionVersion where
  type
    Rs GetSubscriptionDefinitionVersion =
      GetSubscriptionDefinitionVersionResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSubscriptionDefinitionVersionResponse'
            Lude.<$> (x Lude..?> "Definition")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Version")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSubscriptionDefinitionVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetSubscriptionDefinitionVersion where
  toPath GetSubscriptionDefinitionVersion' {..} =
    Lude.mconcat
      [ "/greengrass/definition/subscriptions/",
        Lude.toBS subscriptionDefinitionId,
        "/versions/",
        Lude.toBS subscriptionDefinitionVersionId
      ]

instance Lude.ToQuery GetSubscriptionDefinitionVersion where
  toQuery GetSubscriptionDefinitionVersion' {..} =
    Lude.mconcat ["NextToken" Lude.=: nextToken]

-- | /See:/ 'mkGetSubscriptionDefinitionVersionResponse' smart constructor.
data GetSubscriptionDefinitionVersionResponse = GetSubscriptionDefinitionVersionResponse'
  { -- | Information about the subscription definition version.
    definition :: Lude.Maybe SubscriptionDefinitionVersion,
    -- | The ARN of the subscription definition version.
    arn :: Lude.Maybe Lude.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The time, in milliseconds since the epoch, when the subscription definition version was created.
    creationTimestamp :: Lude.Maybe Lude.Text,
    -- | The version of the subscription definition version.
    version :: Lude.Maybe Lude.Text,
    -- | The ID of the subscription definition version.
    id :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSubscriptionDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- * 'definition' - Information about the subscription definition version.
-- * 'arn' - The ARN of the subscription definition version.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the subscription definition version was created.
-- * 'version' - The version of the subscription definition version.
-- * 'id' - The ID of the subscription definition version.
-- * 'responseStatus' - The response status code.
mkGetSubscriptionDefinitionVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSubscriptionDefinitionVersionResponse
mkGetSubscriptionDefinitionVersionResponse pResponseStatus_ =
  GetSubscriptionDefinitionVersionResponse'
    { definition =
        Lude.Nothing,
      arn = Lude.Nothing,
      nextToken = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the subscription definition version.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvrsDefinition :: Lens.Lens' GetSubscriptionDefinitionVersionResponse (Lude.Maybe SubscriptionDefinitionVersion)
gsdvrsDefinition = Lens.lens (definition :: GetSubscriptionDefinitionVersionResponse -> Lude.Maybe SubscriptionDefinitionVersion) (\s a -> s {definition = a} :: GetSubscriptionDefinitionVersionResponse)
{-# DEPRECATED gsdvrsDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The ARN of the subscription definition version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvrsARN :: Lens.Lens' GetSubscriptionDefinitionVersionResponse (Lude.Maybe Lude.Text)
gsdvrsARN = Lens.lens (arn :: GetSubscriptionDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetSubscriptionDefinitionVersionResponse)
{-# DEPRECATED gsdvrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvrsNextToken :: Lens.Lens' GetSubscriptionDefinitionVersionResponse (Lude.Maybe Lude.Text)
gsdvrsNextToken = Lens.lens (nextToken :: GetSubscriptionDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetSubscriptionDefinitionVersionResponse)
{-# DEPRECATED gsdvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The time, in milliseconds since the epoch, when the subscription definition version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvrsCreationTimestamp :: Lens.Lens' GetSubscriptionDefinitionVersionResponse (Lude.Maybe Lude.Text)
gsdvrsCreationTimestamp = Lens.lens (creationTimestamp :: GetSubscriptionDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: GetSubscriptionDefinitionVersionResponse)
{-# DEPRECATED gsdvrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The version of the subscription definition version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvrsVersion :: Lens.Lens' GetSubscriptionDefinitionVersionResponse (Lude.Maybe Lude.Text)
gsdvrsVersion = Lens.lens (version :: GetSubscriptionDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: GetSubscriptionDefinitionVersionResponse)
{-# DEPRECATED gsdvrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The ID of the subscription definition version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvrsId :: Lens.Lens' GetSubscriptionDefinitionVersionResponse (Lude.Maybe Lude.Text)
gsdvrsId = Lens.lens (id :: GetSubscriptionDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GetSubscriptionDefinitionVersionResponse)
{-# DEPRECATED gsdvrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvrsResponseStatus :: Lens.Lens' GetSubscriptionDefinitionVersionResponse Lude.Int
gsdvrsResponseStatus = Lens.lens (responseStatus :: GetSubscriptionDefinitionVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSubscriptionDefinitionVersionResponse)
{-# DEPRECATED gsdvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
