{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a group. You may provide the initial version of the group or use ''CreateGroupVersion'' at a later time. Tip: You can use the ''gg_group_setup'' package (https://github.com/awslabs/aws-greengrass-group-setup) as a library or command-line application to create and deploy Greengrass groups.
module Network.AWS.Greengrass.CreateGroup
  ( -- * Creating a request
    CreateGroup (..),
    mkCreateGroup,

    -- ** Request lenses
    cgAmznClientToken,
    cgInitialVersion,
    cgName,
    cgTags,

    -- * Destructuring the response
    CreateGroupResponse (..),
    mkCreateGroupResponse,

    -- ** Response lenses
    cgrsLatestVersionARN,
    cgrsARN,
    cgrsName,
    cgrsCreationTimestamp,
    cgrsId,
    cgrsLatestVersion,
    cgrsLastUpdatedTimestamp,
    cgrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { amznClientToken ::
      Lude.Maybe Lude.Text,
    initialVersion :: Lude.Maybe GroupVersion,
    name :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGroup' with the minimum fields required to make a request.
--
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 'initialVersion' - Information about the initial version of the group.
-- * 'name' - The name of the group.
-- * 'tags' - Tag(s) to add to the new resource.
mkCreateGroup ::
  CreateGroup
mkCreateGroup =
  CreateGroup'
    { amznClientToken = Lude.Nothing,
      initialVersion = Lude.Nothing,
      name = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgAmznClientToken :: Lens.Lens' CreateGroup (Lude.Maybe Lude.Text)
cgAmznClientToken = Lens.lens (amznClientToken :: CreateGroup -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateGroup)
{-# DEPRECATED cgAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | Information about the initial version of the group.
--
-- /Note:/ Consider using 'initialVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgInitialVersion :: Lens.Lens' CreateGroup (Lude.Maybe GroupVersion)
cgInitialVersion = Lens.lens (initialVersion :: CreateGroup -> Lude.Maybe GroupVersion) (\s a -> s {initialVersion = a} :: CreateGroup)
{-# DEPRECATED cgInitialVersion "Use generic-lens or generic-optics with 'initialVersion' instead." #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgName :: Lens.Lens' CreateGroup (Lude.Maybe Lude.Text)
cgName = Lens.lens (name :: CreateGroup -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateGroup)
{-# DEPRECATED cgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgTags :: Lens.Lens' CreateGroup (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cgTags = Lens.lens (tags :: CreateGroup -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateGroup)
{-# DEPRECATED cgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateGroup where
  type Rs CreateGroup = CreateGroupResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateGroupResponse'
            Lude.<$> (x Lude..?> "LatestVersionArn")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (x Lude..?> "LatestVersion")
            Lude.<*> (x Lude..?> "LastUpdatedTimestamp")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateGroup where
  toHeaders CreateGroup' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateGroup where
  toJSON CreateGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InitialVersion" Lude..=) Lude.<$> initialVersion,
            ("Name" Lude..=) Lude.<$> name,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateGroup where
  toPath = Lude.const "/greengrass/groups"

instance Lude.ToQuery CreateGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { latestVersionARN ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    creationTimestamp :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    latestVersion :: Lude.Maybe Lude.Text,
    lastUpdatedTimestamp :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateGroupResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the definition.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
-- * 'id' - The ID of the definition.
-- * 'lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
-- * 'latestVersion' - The ID of the latest version associated with the definition.
-- * 'latestVersionARN' - The ARN of the latest version associated with the definition.
-- * 'name' - The name of the definition.
-- * 'responseStatus' - The response status code.
mkCreateGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateGroupResponse
mkCreateGroupResponse pResponseStatus_ =
  CreateGroupResponse'
    { latestVersionARN = Lude.Nothing,
      arn = Lude.Nothing,
      name = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      id = Lude.Nothing,
      latestVersion = Lude.Nothing,
      lastUpdatedTimestamp = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsLatestVersionARN :: Lens.Lens' CreateGroupResponse (Lude.Maybe Lude.Text)
cgrsLatestVersionARN = Lens.lens (latestVersionARN :: CreateGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersionARN = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsLatestVersionARN "Use generic-lens or generic-optics with 'latestVersionARN' instead." #-}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsARN :: Lens.Lens' CreateGroupResponse (Lude.Maybe Lude.Text)
cgrsARN = Lens.lens (arn :: CreateGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsName :: Lens.Lens' CreateGroupResponse (Lude.Maybe Lude.Text)
cgrsName = Lens.lens (name :: CreateGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsCreationTimestamp :: Lens.Lens' CreateGroupResponse (Lude.Maybe Lude.Text)
cgrsCreationTimestamp = Lens.lens (creationTimestamp :: CreateGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsId :: Lens.Lens' CreateGroupResponse (Lude.Maybe Lude.Text)
cgrsId = Lens.lens (id :: CreateGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsLatestVersion :: Lens.Lens' CreateGroupResponse (Lude.Maybe Lude.Text)
cgrsLatestVersion = Lens.lens (latestVersion :: CreateGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsLastUpdatedTimestamp :: Lens.Lens' CreateGroupResponse (Lude.Maybe Lude.Text)
cgrsLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: CreateGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedTimestamp = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsResponseStatus :: Lens.Lens' CreateGroupResponse Lude.Int
cgrsResponseStatus = Lens.lens (responseStatus :: CreateGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
