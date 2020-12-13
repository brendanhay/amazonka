{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.DescribeAccountAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your OpsWorks-CM account attributes.
--
-- This operation is synchronous.
module Network.AWS.OpsWorksCM.DescribeAccountAttributes
  ( -- * Creating a request
    DescribeAccountAttributes (..),
    mkDescribeAccountAttributes,

    -- * Destructuring the response
    DescribeAccountAttributesResponse (..),
    mkDescribeAccountAttributesResponse,

    -- ** Response lenses
    daarsAttributes,
    daarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccountAttributes' with the minimum fields required to make a request.
mkDescribeAccountAttributes ::
  DescribeAccountAttributes
mkDescribeAccountAttributes = DescribeAccountAttributes'

instance Lude.AWSRequest DescribeAccountAttributes where
  type
    Rs DescribeAccountAttributes =
      DescribeAccountAttributesResponse
  request = Req.postJSON opsWorksCMService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAccountAttributesResponse'
            Lude.<$> (x Lude..?> "Attributes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAccountAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OpsWorksCM_V2016_11_01.DescribeAccountAttributes" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAccountAttributes where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DescribeAccountAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAccountAttributes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { -- | The attributes that are currently set for the account.
    attributes :: Lude.Maybe [AccountAttribute],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccountAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - The attributes that are currently set for the account.
-- * 'responseStatus' - The response status code.
mkDescribeAccountAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAccountAttributesResponse
mkDescribeAccountAttributesResponse pResponseStatus_ =
  DescribeAccountAttributesResponse'
    { attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The attributes that are currently set for the account.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsAttributes :: Lens.Lens' DescribeAccountAttributesResponse (Lude.Maybe [AccountAttribute])
daarsAttributes = Lens.lens (attributes :: DescribeAccountAttributesResponse -> Lude.Maybe [AccountAttribute]) (\s a -> s {attributes = a} :: DescribeAccountAttributesResponse)
{-# DEPRECATED daarsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsResponseStatus :: Lens.Lens' DescribeAccountAttributesResponse Lude.Int
daarsResponseStatus = Lens.lens (responseStatus :: DescribeAccountAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAccountAttributesResponse)
{-# DEPRECATED daarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
