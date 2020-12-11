{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateCoreDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a core definition that has already been defined. Greengrass groups must each contain exactly one Greengrass core.
module Network.AWS.Greengrass.CreateCoreDefinitionVersion
  ( -- * Creating a request
    CreateCoreDefinitionVersion (..),
    mkCreateCoreDefinitionVersion,

    -- ** Request lenses
    creAmznClientToken,
    creCores,
    creCoreDefinitionId,

    -- * Destructuring the response
    CreateCoreDefinitionVersionResponse (..),
    mkCreateCoreDefinitionVersionResponse,

    -- ** Response lenses
    crsARN,
    crsCreationTimestamp,
    crsVersion,
    crsId,
    crsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateCoreDefinitionVersion' smart constructor.
data CreateCoreDefinitionVersion = CreateCoreDefinitionVersion'
  { amznClientToken ::
      Lude.Maybe Lude.Text,
    cores :: Lude.Maybe [Core],
    coreDefinitionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCoreDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 'coreDefinitionId' - The ID of the core definition.
-- * 'cores' - A list of cores in the core definition version.
mkCreateCoreDefinitionVersion ::
  -- | 'coreDefinitionId'
  Lude.Text ->
  CreateCoreDefinitionVersion
mkCreateCoreDefinitionVersion pCoreDefinitionId_ =
  CreateCoreDefinitionVersion'
    { amznClientToken = Lude.Nothing,
      cores = Lude.Nothing,
      coreDefinitionId = pCoreDefinitionId_
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creAmznClientToken :: Lens.Lens' CreateCoreDefinitionVersion (Lude.Maybe Lude.Text)
creAmznClientToken = Lens.lens (amznClientToken :: CreateCoreDefinitionVersion -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateCoreDefinitionVersion)
{-# DEPRECATED creAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | A list of cores in the core definition version.
--
-- /Note:/ Consider using 'cores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creCores :: Lens.Lens' CreateCoreDefinitionVersion (Lude.Maybe [Core])
creCores = Lens.lens (cores :: CreateCoreDefinitionVersion -> Lude.Maybe [Core]) (\s a -> s {cores = a} :: CreateCoreDefinitionVersion)
{-# DEPRECATED creCores "Use generic-lens or generic-optics with 'cores' instead." #-}

-- | The ID of the core definition.
--
-- /Note:/ Consider using 'coreDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creCoreDefinitionId :: Lens.Lens' CreateCoreDefinitionVersion Lude.Text
creCoreDefinitionId = Lens.lens (coreDefinitionId :: CreateCoreDefinitionVersion -> Lude.Text) (\s a -> s {coreDefinitionId = a} :: CreateCoreDefinitionVersion)
{-# DEPRECATED creCoreDefinitionId "Use generic-lens or generic-optics with 'coreDefinitionId' instead." #-}

instance Lude.AWSRequest CreateCoreDefinitionVersion where
  type
    Rs CreateCoreDefinitionVersion =
      CreateCoreDefinitionVersionResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCoreDefinitionVersionResponse'
            Lude.<$> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Version")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCoreDefinitionVersion where
  toHeaders CreateCoreDefinitionVersion' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateCoreDefinitionVersion where
  toJSON CreateCoreDefinitionVersion' {..} =
    Lude.object (Lude.catMaybes [("Cores" Lude..=) Lude.<$> cores])

instance Lude.ToPath CreateCoreDefinitionVersion where
  toPath CreateCoreDefinitionVersion' {..} =
    Lude.mconcat
      [ "/greengrass/definition/cores/",
        Lude.toBS coreDefinitionId,
        "/versions"
      ]

instance Lude.ToQuery CreateCoreDefinitionVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCoreDefinitionVersionResponse' smart constructor.
data CreateCoreDefinitionVersionResponse = CreateCoreDefinitionVersionResponse'
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

-- | Creates a value of 'CreateCoreDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the version.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
-- * 'id' - The ID of the parent definition that the version is associated with.
-- * 'responseStatus' - The response status code.
-- * 'version' - The ID of the version.
mkCreateCoreDefinitionVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCoreDefinitionVersionResponse
mkCreateCoreDefinitionVersionResponse pResponseStatus_ =
  CreateCoreDefinitionVersionResponse'
    { arn = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsARN :: Lens.Lens' CreateCoreDefinitionVersionResponse (Lude.Maybe Lude.Text)
crsARN = Lens.lens (arn :: CreateCoreDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateCoreDefinitionVersionResponse)
{-# DEPRECATED crsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsCreationTimestamp :: Lens.Lens' CreateCoreDefinitionVersionResponse (Lude.Maybe Lude.Text)
crsCreationTimestamp = Lens.lens (creationTimestamp :: CreateCoreDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: CreateCoreDefinitionVersionResponse)
{-# DEPRECATED crsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsVersion :: Lens.Lens' CreateCoreDefinitionVersionResponse (Lude.Maybe Lude.Text)
crsVersion = Lens.lens (version :: CreateCoreDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: CreateCoreDefinitionVersionResponse)
{-# DEPRECATED crsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The ID of the parent definition that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsId :: Lens.Lens' CreateCoreDefinitionVersionResponse (Lude.Maybe Lude.Text)
crsId = Lens.lens (id :: CreateCoreDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateCoreDefinitionVersionResponse)
{-# DEPRECATED crsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateCoreDefinitionVersionResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CreateCoreDefinitionVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCoreDefinitionVersionResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
