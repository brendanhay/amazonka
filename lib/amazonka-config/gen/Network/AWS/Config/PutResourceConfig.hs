{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records the configuration state for the resource provided in the request. The configuration state of a resource is represented in AWS Config as Configuration Items. Once this API records the configuration item, you can retrieve the list of configuration items for the custom resource type using existing AWS Config APIs.
module Network.AWS.Config.PutResourceConfig
  ( -- * Creating a request
    PutResourceConfig (..),
    mkPutResourceConfig,

    -- ** Request lenses
    prcResourceName,
    prcTags,
    prcResourceType,
    prcSchemaVersionId,
    prcResourceId,
    prcConfiguration,

    -- * Destructuring the response
    PutResourceConfigResponse (..),
    mkPutResourceConfigResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutResourceConfig' smart constructor.
data PutResourceConfig = PutResourceConfig'
  { resourceName ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    resourceType :: Lude.Text,
    schemaVersionId :: Lude.Text,
    resourceId :: Lude.Text,
    configuration :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutResourceConfig' with the minimum fields required to make a request.
--
-- * 'configuration' - The configuration object of the resource in valid JSON format. It must match the schema registered with AWS CloudFormation.
-- * 'resourceId' - Unique identifier of the resource.
-- * 'resourceName' - Name of the resource.
-- * 'resourceType' - The type of the resource. The custom resource type must be registered with AWS CloudFormation.
-- * 'schemaVersionId' - Version of the schema registered for the ResourceType in AWS CloudFormation.
-- * 'tags' - Tags associated with the resource.
mkPutResourceConfig ::
  -- | 'resourceType'
  Lude.Text ->
  -- | 'schemaVersionId'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  -- | 'configuration'
  Lude.Text ->
  PutResourceConfig
mkPutResourceConfig
  pResourceType_
  pSchemaVersionId_
  pResourceId_
  pConfiguration_ =
    PutResourceConfig'
      { resourceName = Lude.Nothing,
        tags = Lude.Nothing,
        resourceType = pResourceType_,
        schemaVersionId = pSchemaVersionId_,
        resourceId = pResourceId_,
        configuration = pConfiguration_
      }

-- | Name of the resource.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcResourceName :: Lens.Lens' PutResourceConfig (Lude.Maybe Lude.Text)
prcResourceName = Lens.lens (resourceName :: PutResourceConfig -> Lude.Maybe Lude.Text) (\s a -> s {resourceName = a} :: PutResourceConfig)
{-# DEPRECATED prcResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | Tags associated with the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcTags :: Lens.Lens' PutResourceConfig (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
prcTags = Lens.lens (tags :: PutResourceConfig -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: PutResourceConfig)
{-# DEPRECATED prcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The type of the resource. The custom resource type must be registered with AWS CloudFormation.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcResourceType :: Lens.Lens' PutResourceConfig Lude.Text
prcResourceType = Lens.lens (resourceType :: PutResourceConfig -> Lude.Text) (\s a -> s {resourceType = a} :: PutResourceConfig)
{-# DEPRECATED prcResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Version of the schema registered for the ResourceType in AWS CloudFormation.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcSchemaVersionId :: Lens.Lens' PutResourceConfig Lude.Text
prcSchemaVersionId = Lens.lens (schemaVersionId :: PutResourceConfig -> Lude.Text) (\s a -> s {schemaVersionId = a} :: PutResourceConfig)
{-# DEPRECATED prcSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | Unique identifier of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcResourceId :: Lens.Lens' PutResourceConfig Lude.Text
prcResourceId = Lens.lens (resourceId :: PutResourceConfig -> Lude.Text) (\s a -> s {resourceId = a} :: PutResourceConfig)
{-# DEPRECATED prcResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The configuration object of the resource in valid JSON format. It must match the schema registered with AWS CloudFormation.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcConfiguration :: Lens.Lens' PutResourceConfig Lude.Text
prcConfiguration = Lens.lens (configuration :: PutResourceConfig -> Lude.Text) (\s a -> s {configuration = a} :: PutResourceConfig)
{-# DEPRECATED prcConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

instance Lude.AWSRequest PutResourceConfig where
  type Rs PutResourceConfig = PutResourceConfigResponse
  request = Req.postJSON configService
  response = Res.receiveNull PutResourceConfigResponse'

instance Lude.ToHeaders PutResourceConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StarlingDoveService.PutResourceConfig" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutResourceConfig where
  toJSON PutResourceConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceName" Lude..=) Lude.<$> resourceName,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("ResourceType" Lude..= resourceType),
            Lude.Just ("SchemaVersionId" Lude..= schemaVersionId),
            Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("Configuration" Lude..= configuration)
          ]
      )

instance Lude.ToPath PutResourceConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery PutResourceConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutResourceConfigResponse' smart constructor.
data PutResourceConfigResponse = PutResourceConfigResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutResourceConfigResponse' with the minimum fields required to make a request.
mkPutResourceConfigResponse ::
  PutResourceConfigResponse
mkPutResourceConfigResponse = PutResourceConfigResponse'
