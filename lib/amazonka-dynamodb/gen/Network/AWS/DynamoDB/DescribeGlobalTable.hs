{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeGlobalTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified global table.
module Network.AWS.DynamoDB.DescribeGlobalTable
  ( -- * Creating a request
    DescribeGlobalTable (..),
    mkDescribeGlobalTable,

    -- ** Request lenses
    dgtGlobalTableName,

    -- * Destructuring the response
    DescribeGlobalTableResponse (..),
    mkDescribeGlobalTableResponse,

    -- ** Response lenses
    dgtrsGlobalTableDescription,
    dgtrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeGlobalTable' smart constructor.
newtype DescribeGlobalTable = DescribeGlobalTable'
  { globalTableName ::
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

-- | Creates a value of 'DescribeGlobalTable' with the minimum fields required to make a request.
--
-- * 'globalTableName' - The name of the global table.
mkDescribeGlobalTable ::
  -- | 'globalTableName'
  Lude.Text ->
  DescribeGlobalTable
mkDescribeGlobalTable pGlobalTableName_ =
  DescribeGlobalTable' {globalTableName = pGlobalTableName_}

-- | The name of the global table.
--
-- /Note:/ Consider using 'globalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgtGlobalTableName :: Lens.Lens' DescribeGlobalTable Lude.Text
dgtGlobalTableName = Lens.lens (globalTableName :: DescribeGlobalTable -> Lude.Text) (\s a -> s {globalTableName = a} :: DescribeGlobalTable)
{-# DEPRECATED dgtGlobalTableName "Use generic-lens or generic-optics with 'globalTableName' instead." #-}

instance Lude.AWSRequest DescribeGlobalTable where
  type Rs DescribeGlobalTable = DescribeGlobalTableResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeGlobalTableResponse'
            Lude.<$> (x Lude..?> "GlobalTableDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeGlobalTable where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.DescribeGlobalTable" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeGlobalTable where
  toJSON DescribeGlobalTable' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("GlobalTableName" Lude..= globalTableName)]
      )

instance Lude.ToPath DescribeGlobalTable where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeGlobalTable where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeGlobalTableResponse' smart constructor.
data DescribeGlobalTableResponse = DescribeGlobalTableResponse'
  { globalTableDescription ::
      Lude.Maybe GlobalTableDescription,
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

-- | Creates a value of 'DescribeGlobalTableResponse' with the minimum fields required to make a request.
--
-- * 'globalTableDescription' - Contains the details of the global table.
-- * 'responseStatus' - The response status code.
mkDescribeGlobalTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeGlobalTableResponse
mkDescribeGlobalTableResponse pResponseStatus_ =
  DescribeGlobalTableResponse'
    { globalTableDescription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains the details of the global table.
--
-- /Note:/ Consider using 'globalTableDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgtrsGlobalTableDescription :: Lens.Lens' DescribeGlobalTableResponse (Lude.Maybe GlobalTableDescription)
dgtrsGlobalTableDescription = Lens.lens (globalTableDescription :: DescribeGlobalTableResponse -> Lude.Maybe GlobalTableDescription) (\s a -> s {globalTableDescription = a} :: DescribeGlobalTableResponse)
{-# DEPRECATED dgtrsGlobalTableDescription "Use generic-lens or generic-optics with 'globalTableDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgtrsResponseStatus :: Lens.Lens' DescribeGlobalTableResponse Lude.Int
dgtrsResponseStatus = Lens.lens (responseStatus :: DescribeGlobalTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeGlobalTableResponse)
{-# DEPRECATED dgtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
