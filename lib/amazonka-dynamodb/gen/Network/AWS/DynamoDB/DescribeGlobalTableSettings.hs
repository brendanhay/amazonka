{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeGlobalTableSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Region-specific settings for a global table.
module Network.AWS.DynamoDB.DescribeGlobalTableSettings
  ( -- * Creating a request
    DescribeGlobalTableSettings (..),
    mkDescribeGlobalTableSettings,

    -- ** Request lenses
    dgtsGlobalTableName,

    -- * Destructuring the response
    DescribeGlobalTableSettingsResponse (..),
    mkDescribeGlobalTableSettingsResponse,

    -- ** Response lenses
    dgtsrsReplicaSettings,
    dgtsrsGlobalTableName,
    dgtsrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeGlobalTableSettings' smart constructor.
newtype DescribeGlobalTableSettings = DescribeGlobalTableSettings'
  { -- | The name of the global table to describe.
    globalTableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGlobalTableSettings' with the minimum fields required to make a request.
--
-- * 'globalTableName' - The name of the global table to describe.
mkDescribeGlobalTableSettings ::
  -- | 'globalTableName'
  Lude.Text ->
  DescribeGlobalTableSettings
mkDescribeGlobalTableSettings pGlobalTableName_ =
  DescribeGlobalTableSettings' {globalTableName = pGlobalTableName_}

-- | The name of the global table to describe.
--
-- /Note:/ Consider using 'globalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgtsGlobalTableName :: Lens.Lens' DescribeGlobalTableSettings Lude.Text
dgtsGlobalTableName = Lens.lens (globalTableName :: DescribeGlobalTableSettings -> Lude.Text) (\s a -> s {globalTableName = a} :: DescribeGlobalTableSettings)
{-# DEPRECATED dgtsGlobalTableName "Use generic-lens or generic-optics with 'globalTableName' instead." #-}

instance Lude.AWSRequest DescribeGlobalTableSettings where
  type
    Rs DescribeGlobalTableSettings =
      DescribeGlobalTableSettingsResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeGlobalTableSettingsResponse'
            Lude.<$> (x Lude..?> "ReplicaSettings" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "GlobalTableName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeGlobalTableSettings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DynamoDB_20120810.DescribeGlobalTableSettings" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeGlobalTableSettings where
  toJSON DescribeGlobalTableSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("GlobalTableName" Lude..= globalTableName)]
      )

instance Lude.ToPath DescribeGlobalTableSettings where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeGlobalTableSettings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeGlobalTableSettingsResponse' smart constructor.
data DescribeGlobalTableSettingsResponse = DescribeGlobalTableSettingsResponse'
  { -- | The Region-specific settings for the global table.
    replicaSettings :: Lude.Maybe [ReplicaSettingsDescription],
    -- | The name of the global table.
    globalTableName :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGlobalTableSettingsResponse' with the minimum fields required to make a request.
--
-- * 'replicaSettings' - The Region-specific settings for the global table.
-- * 'globalTableName' - The name of the global table.
-- * 'responseStatus' - The response status code.
mkDescribeGlobalTableSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeGlobalTableSettingsResponse
mkDescribeGlobalTableSettingsResponse pResponseStatus_ =
  DescribeGlobalTableSettingsResponse'
    { replicaSettings =
        Lude.Nothing,
      globalTableName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Region-specific settings for the global table.
--
-- /Note:/ Consider using 'replicaSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgtsrsReplicaSettings :: Lens.Lens' DescribeGlobalTableSettingsResponse (Lude.Maybe [ReplicaSettingsDescription])
dgtsrsReplicaSettings = Lens.lens (replicaSettings :: DescribeGlobalTableSettingsResponse -> Lude.Maybe [ReplicaSettingsDescription]) (\s a -> s {replicaSettings = a} :: DescribeGlobalTableSettingsResponse)
{-# DEPRECATED dgtsrsReplicaSettings "Use generic-lens or generic-optics with 'replicaSettings' instead." #-}

-- | The name of the global table.
--
-- /Note:/ Consider using 'globalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgtsrsGlobalTableName :: Lens.Lens' DescribeGlobalTableSettingsResponse (Lude.Maybe Lude.Text)
dgtsrsGlobalTableName = Lens.lens (globalTableName :: DescribeGlobalTableSettingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {globalTableName = a} :: DescribeGlobalTableSettingsResponse)
{-# DEPRECATED dgtsrsGlobalTableName "Use generic-lens or generic-optics with 'globalTableName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgtsrsResponseStatus :: Lens.Lens' DescribeGlobalTableSettingsResponse Lude.Int
dgtsrsResponseStatus = Lens.lens (responseStatus :: DescribeGlobalTableSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeGlobalTableSettingsResponse)
{-# DEPRECATED dgtsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
