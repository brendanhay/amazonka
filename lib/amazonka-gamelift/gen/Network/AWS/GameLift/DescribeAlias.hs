{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties for an alias. This operation returns all alias metadata and settings. To get an alias's target fleet ID only, use @ResolveAlias@ .
--
-- To get alias properties, specify the alias ID. If successful, the requested alias record is returned.
--
--     * 'CreateAlias'
--
--
--     * 'ListAliases'
--
--
--     * 'DescribeAlias'
--
--
--     * 'UpdateAlias'
--
--
--     * 'DeleteAlias'
--
--
--     * 'ResolveAlias'
module Network.AWS.GameLift.DescribeAlias
  ( -- * Creating a request
    DescribeAlias (..),
    mkDescribeAlias,

    -- ** Request lenses
    daAliasId,

    -- * Destructuring the response
    DescribeAliasResponse (..),
    mkDescribeAliasResponse,

    -- ** Response lenses
    darsAlias,
    darsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeAlias' smart constructor.
newtype DescribeAlias = DescribeAlias'
  { -- | The unique identifier for the fleet alias that you want to retrieve. You can use either the alias ID or ARN value.
    aliasId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAlias' with the minimum fields required to make a request.
--
-- * 'aliasId' - The unique identifier for the fleet alias that you want to retrieve. You can use either the alias ID or ARN value.
mkDescribeAlias ::
  -- | 'aliasId'
  Lude.Text ->
  DescribeAlias
mkDescribeAlias pAliasId_ = DescribeAlias' {aliasId = pAliasId_}

-- | The unique identifier for the fleet alias that you want to retrieve. You can use either the alias ID or ARN value.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAliasId :: Lens.Lens' DescribeAlias Lude.Text
daAliasId = Lens.lens (aliasId :: DescribeAlias -> Lude.Text) (\s a -> s {aliasId = a} :: DescribeAlias)
{-# DEPRECATED daAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

instance Lude.AWSRequest DescribeAlias where
  type Rs DescribeAlias = DescribeAliasResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAliasResponse'
            Lude.<$> (x Lude..?> "Alias") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAlias where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeAlias" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAlias where
  toJSON DescribeAlias' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("AliasId" Lude..= aliasId)])

instance Lude.ToPath DescribeAlias where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAlias where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeAliasResponse' smart constructor.
data DescribeAliasResponse = DescribeAliasResponse'
  { -- | The requested alias resource.
    alias :: Lude.Maybe Alias,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAliasResponse' with the minimum fields required to make a request.
--
-- * 'alias' - The requested alias resource.
-- * 'responseStatus' - The response status code.
mkDescribeAliasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAliasResponse
mkDescribeAliasResponse pResponseStatus_ =
  DescribeAliasResponse'
    { alias = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The requested alias resource.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsAlias :: Lens.Lens' DescribeAliasResponse (Lude.Maybe Alias)
darsAlias = Lens.lens (alias :: DescribeAliasResponse -> Lude.Maybe Alias) (\s a -> s {alias = a} :: DescribeAliasResponse)
{-# DEPRECATED darsAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DescribeAliasResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DescribeAliasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAliasResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
