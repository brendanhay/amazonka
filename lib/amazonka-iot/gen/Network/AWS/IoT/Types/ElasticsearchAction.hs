{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ElasticsearchAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ElasticsearchAction
  ( ElasticsearchAction (..),

    -- * Smart constructor
    mkElasticsearchAction,

    -- * Lenses
    eaRoleArn,
    eaEndpoint,
    eaIndex,
    eaType,
    eaId,
  )
where

import qualified Network.AWS.IoT.Types.AwsArn as Types
import qualified Network.AWS.IoT.Types.Endpoint as Types
import qualified Network.AWS.IoT.Types.Id as Types
import qualified Network.AWS.IoT.Types.Index as Types
import qualified Network.AWS.IoT.Types.Type as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an action that writes data to an Amazon Elasticsearch Service domain.
--
-- /See:/ 'mkElasticsearchAction' smart constructor.
data ElasticsearchAction = ElasticsearchAction'
  { -- | The IAM role ARN that has access to Elasticsearch.
    roleArn :: Types.AwsArn,
    -- | The endpoint of your Elasticsearch domain.
    endpoint :: Types.Endpoint,
    -- | The Elasticsearch index where you want to store your data.
    index :: Types.Index,
    -- | The type of document you are storing.
    type' :: Types.Type,
    -- | The unique identifier for the document you are storing.
    id :: Types.Id
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ElasticsearchAction' value with any optional fields omitted.
mkElasticsearchAction ::
  -- | 'roleArn'
  Types.AwsArn ->
  -- | 'endpoint'
  Types.Endpoint ->
  -- | 'index'
  Types.Index ->
  -- | 'type\''
  Types.Type ->
  -- | 'id'
  Types.Id ->
  ElasticsearchAction
mkElasticsearchAction roleArn endpoint index type' id =
  ElasticsearchAction' {roleArn, endpoint, index, type', id}

-- | The IAM role ARN that has access to Elasticsearch.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaRoleArn :: Lens.Lens' ElasticsearchAction Types.AwsArn
eaRoleArn = Lens.field @"roleArn"
{-# DEPRECATED eaRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The endpoint of your Elasticsearch domain.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaEndpoint :: Lens.Lens' ElasticsearchAction Types.Endpoint
eaEndpoint = Lens.field @"endpoint"
{-# DEPRECATED eaEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The Elasticsearch index where you want to store your data.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaIndex :: Lens.Lens' ElasticsearchAction Types.Index
eaIndex = Lens.field @"index"
{-# DEPRECATED eaIndex "Use generic-lens or generic-optics with 'index' instead." #-}

-- | The type of document you are storing.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaType :: Lens.Lens' ElasticsearchAction Types.Type
eaType = Lens.field @"type'"
{-# DEPRECATED eaType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The unique identifier for the document you are storing.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaId :: Lens.Lens' ElasticsearchAction Types.Id
eaId = Lens.field @"id"
{-# DEPRECATED eaId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromJSON ElasticsearchAction where
  toJSON ElasticsearchAction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("roleArn" Core..= roleArn),
            Core.Just ("endpoint" Core..= endpoint),
            Core.Just ("index" Core..= index),
            Core.Just ("type" Core..= type'),
            Core.Just ("id" Core..= id)
          ]
      )

instance Core.FromJSON ElasticsearchAction where
  parseJSON =
    Core.withObject "ElasticsearchAction" Core.$
      \x ->
        ElasticsearchAction'
          Core.<$> (x Core..: "roleArn")
          Core.<*> (x Core..: "endpoint")
          Core.<*> (x Core..: "index")
          Core.<*> (x Core..: "type")
          Core.<*> (x Core..: "id")
