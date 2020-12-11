{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateRegistry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new registry which may be used to hold a collection of schemas.
module Network.AWS.Glue.CreateRegistry
  ( -- * Creating a request
    CreateRegistry (..),
    mkCreateRegistry,

    -- ** Request lenses
    crDescription,
    crTags,
    crRegistryName,

    -- * Destructuring the response
    CreateRegistryResponse (..),
    mkCreateRegistryResponse,

    -- ** Response lenses
    crrsRegistryName,
    crrsRegistryARN,
    crrsDescription,
    crrsTags,
    crrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateRegistry' smart constructor.
data CreateRegistry = CreateRegistry'
  { description ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    registryName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRegistry' with the minimum fields required to make a request.
--
-- * 'description' - A description of the registry. If description is not provided, there will not be any default value for this.
-- * 'registryName' - Name of the registry to be created of max length of 255, and may only contain letters, numbers, hyphen, underscore, dollar sign, or hash mark. No whitespace.
-- * 'tags' - AWS tags that contain a key value pair and may be searched by console, command line, or API.
mkCreateRegistry ::
  -- | 'registryName'
  Lude.Text ->
  CreateRegistry
mkCreateRegistry pRegistryName_ =
  CreateRegistry'
    { description = Lude.Nothing,
      tags = Lude.Nothing,
      registryName = pRegistryName_
    }

-- | A description of the registry. If description is not provided, there will not be any default value for this.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDescription :: Lens.Lens' CreateRegistry (Lude.Maybe Lude.Text)
crDescription = Lens.lens (description :: CreateRegistry -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateRegistry)
{-# DEPRECATED crDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | AWS tags that contain a key value pair and may be searched by console, command line, or API.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' CreateRegistry (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
crTags = Lens.lens (tags :: CreateRegistry -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateRegistry)
{-# DEPRECATED crTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Name of the registry to be created of max length of 255, and may only contain letters, numbers, hyphen, underscore, dollar sign, or hash mark. No whitespace.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRegistryName :: Lens.Lens' CreateRegistry Lude.Text
crRegistryName = Lens.lens (registryName :: CreateRegistry -> Lude.Text) (\s a -> s {registryName = a} :: CreateRegistry)
{-# DEPRECATED crRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

instance Lude.AWSRequest CreateRegistry where
  type Rs CreateRegistry = CreateRegistryResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateRegistryResponse'
            Lude.<$> (x Lude..?> "RegistryName")
            Lude.<*> (x Lude..?> "RegistryArn")
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateRegistry where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.CreateRegistry" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateRegistry where
  toJSON CreateRegistry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("RegistryName" Lude..= registryName)
          ]
      )

instance Lude.ToPath CreateRegistry where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateRegistry where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateRegistryResponse' smart constructor.
data CreateRegistryResponse = CreateRegistryResponse'
  { registryName ::
      Lude.Maybe Lude.Text,
    registryARN :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    tags ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
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

-- | Creates a value of 'CreateRegistryResponse' with the minimum fields required to make a request.
--
-- * 'description' - A description of the registry.
-- * 'registryARN' - The Amazon Resource Name (ARN) of the newly created registry.
-- * 'registryName' - The name of the registry.
-- * 'responseStatus' - The response status code.
-- * 'tags' - The tags for the registry.
mkCreateRegistryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateRegistryResponse
mkCreateRegistryResponse pResponseStatus_ =
  CreateRegistryResponse'
    { registryName = Lude.Nothing,
      registryARN = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the registry.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsRegistryName :: Lens.Lens' CreateRegistryResponse (Lude.Maybe Lude.Text)
crrsRegistryName = Lens.lens (registryName :: CreateRegistryResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryName = a} :: CreateRegistryResponse)
{-# DEPRECATED crrsRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | The Amazon Resource Name (ARN) of the newly created registry.
--
-- /Note:/ Consider using 'registryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsRegistryARN :: Lens.Lens' CreateRegistryResponse (Lude.Maybe Lude.Text)
crrsRegistryARN = Lens.lens (registryARN :: CreateRegistryResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryARN = a} :: CreateRegistryResponse)
{-# DEPRECATED crrsRegistryARN "Use generic-lens or generic-optics with 'registryARN' instead." #-}

-- | A description of the registry.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsDescription :: Lens.Lens' CreateRegistryResponse (Lude.Maybe Lude.Text)
crrsDescription = Lens.lens (description :: CreateRegistryResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateRegistryResponse)
{-# DEPRECATED crrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags for the registry.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsTags :: Lens.Lens' CreateRegistryResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
crrsTags = Lens.lens (tags :: CreateRegistryResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateRegistryResponse)
{-# DEPRECATED crrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsResponseStatus :: Lens.Lens' CreateRegistryResponse Lude.Int
crrsResponseStatus = Lens.lens (responseStatus :: CreateRegistryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRegistryResponse)
{-# DEPRECATED crrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
