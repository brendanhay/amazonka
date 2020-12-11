{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeClientProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified Amazon WorkSpaces clients.
module Network.AWS.WorkSpaces.DescribeClientProperties
  ( -- * Creating a request
    DescribeClientProperties (..),
    mkDescribeClientProperties,

    -- ** Request lenses
    dcpResourceIds,

    -- * Destructuring the response
    DescribeClientPropertiesResponse (..),
    mkDescribeClientPropertiesResponse,

    -- ** Response lenses
    dcprsClientPropertiesList,
    dcprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDescribeClientProperties' smart constructor.
newtype DescribeClientProperties = DescribeClientProperties'
  { resourceIds ::
      Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClientProperties' with the minimum fields required to make a request.
--
-- * 'resourceIds' - The resource identifier, in the form of directory IDs.
mkDescribeClientProperties ::
  -- | 'resourceIds'
  Lude.NonEmpty Lude.Text ->
  DescribeClientProperties
mkDescribeClientProperties pResourceIds_ =
  DescribeClientProperties' {resourceIds = pResourceIds_}

-- | The resource identifier, in the form of directory IDs.
--
-- /Note:/ Consider using 'resourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpResourceIds :: Lens.Lens' DescribeClientProperties (Lude.NonEmpty Lude.Text)
dcpResourceIds = Lens.lens (resourceIds :: DescribeClientProperties -> Lude.NonEmpty Lude.Text) (\s a -> s {resourceIds = a} :: DescribeClientProperties)
{-# DEPRECATED dcpResourceIds "Use generic-lens or generic-optics with 'resourceIds' instead." #-}

instance Lude.AWSRequest DescribeClientProperties where
  type Rs DescribeClientProperties = DescribeClientPropertiesResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeClientPropertiesResponse'
            Lude.<$> (x Lude..?> "ClientPropertiesList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeClientProperties where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.DescribeClientProperties" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeClientProperties where
  toJSON DescribeClientProperties' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ResourceIds" Lude..= resourceIds)])

instance Lude.ToPath DescribeClientProperties where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeClientProperties where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeClientPropertiesResponse' smart constructor.
data DescribeClientPropertiesResponse = DescribeClientPropertiesResponse'
  { clientPropertiesList ::
      Lude.Maybe
        [ClientPropertiesResult],
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

-- | Creates a value of 'DescribeClientPropertiesResponse' with the minimum fields required to make a request.
--
-- * 'clientPropertiesList' - Information about the specified Amazon WorkSpaces clients.
-- * 'responseStatus' - The response status code.
mkDescribeClientPropertiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeClientPropertiesResponse
mkDescribeClientPropertiesResponse pResponseStatus_ =
  DescribeClientPropertiesResponse'
    { clientPropertiesList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the specified Amazon WorkSpaces clients.
--
-- /Note:/ Consider using 'clientPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsClientPropertiesList :: Lens.Lens' DescribeClientPropertiesResponse (Lude.Maybe [ClientPropertiesResult])
dcprsClientPropertiesList = Lens.lens (clientPropertiesList :: DescribeClientPropertiesResponse -> Lude.Maybe [ClientPropertiesResult]) (\s a -> s {clientPropertiesList = a} :: DescribeClientPropertiesResponse)
{-# DEPRECATED dcprsClientPropertiesList "Use generic-lens or generic-optics with 'clientPropertiesList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsResponseStatus :: Lens.Lens' DescribeClientPropertiesResponse Lude.Int
dcprsResponseStatus = Lens.lens (responseStatus :: DescribeClientPropertiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClientPropertiesResponse)
{-# DEPRECATED dcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
