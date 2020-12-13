{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DescribeInstanceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified instance attribute.
module Network.AWS.Connect.DescribeInstanceAttribute
  ( -- * Creating a request
    DescribeInstanceAttribute (..),
    mkDescribeInstanceAttribute,

    -- ** Request lenses
    diaInstanceId,
    diaAttributeType,

    -- * Destructuring the response
    DescribeInstanceAttributeResponse (..),
    mkDescribeInstanceAttributeResponse,

    -- ** Response lenses
    diarsAttribute,
    diarsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeInstanceAttribute' smart constructor.
data DescribeInstanceAttribute = DescribeInstanceAttribute'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The type of attribute.
    attributeType :: InstanceAttributeType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceAttribute' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'attributeType' - The type of attribute.
mkDescribeInstanceAttribute ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'attributeType'
  InstanceAttributeType ->
  DescribeInstanceAttribute
mkDescribeInstanceAttribute pInstanceId_ pAttributeType_ =
  DescribeInstanceAttribute'
    { instanceId = pInstanceId_,
      attributeType = pAttributeType_
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaInstanceId :: Lens.Lens' DescribeInstanceAttribute Lude.Text
diaInstanceId = Lens.lens (instanceId :: DescribeInstanceAttribute -> Lude.Text) (\s a -> s {instanceId = a} :: DescribeInstanceAttribute)
{-# DEPRECATED diaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The type of attribute.
--
-- /Note:/ Consider using 'attributeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaAttributeType :: Lens.Lens' DescribeInstanceAttribute InstanceAttributeType
diaAttributeType = Lens.lens (attributeType :: DescribeInstanceAttribute -> InstanceAttributeType) (\s a -> s {attributeType = a} :: DescribeInstanceAttribute)
{-# DEPRECATED diaAttributeType "Use generic-lens or generic-optics with 'attributeType' instead." #-}

instance Lude.AWSRequest DescribeInstanceAttribute where
  type
    Rs DescribeInstanceAttribute =
      DescribeInstanceAttributeResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeInstanceAttributeResponse'
            Lude.<$> (x Lude..?> "Attribute") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstanceAttribute where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeInstanceAttribute where
  toPath DescribeInstanceAttribute' {..} =
    Lude.mconcat
      [ "/instance/",
        Lude.toBS instanceId,
        "/attribute/",
        Lude.toBS attributeType
      ]

instance Lude.ToQuery DescribeInstanceAttribute where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeInstanceAttributeResponse' smart constructor.
data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse'
  { -- | The type of attribute.
    attribute :: Lude.Maybe Attribute,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceAttributeResponse' with the minimum fields required to make a request.
--
-- * 'attribute' - The type of attribute.
-- * 'responseStatus' - The response status code.
mkDescribeInstanceAttributeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstanceAttributeResponse
mkDescribeInstanceAttributeResponse pResponseStatus_ =
  DescribeInstanceAttributeResponse'
    { attribute = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The type of attribute.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsAttribute :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe Attribute)
diarsAttribute = Lens.lens (attribute :: DescribeInstanceAttributeResponse -> Lude.Maybe Attribute) (\s a -> s {attribute = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED diarsAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsResponseStatus :: Lens.Lens' DescribeInstanceAttributeResponse Lude.Int
diarsResponseStatus = Lens.lens (responseStatus :: DescribeInstanceAttributeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED diarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
