{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateInstanceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the value for the specified attribute type.
module Network.AWS.Connect.UpdateInstanceAttribute
  ( -- * Creating a request
    UpdateInstanceAttribute (..),
    mkUpdateInstanceAttribute,

    -- ** Request lenses
    uiaInstanceId,
    uiaAttributeType,
    uiaValue,

    -- * Destructuring the response
    UpdateInstanceAttributeResponse (..),
    mkUpdateInstanceAttributeResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateInstanceAttribute' smart constructor.
data UpdateInstanceAttribute = UpdateInstanceAttribute'
  { instanceId ::
      Lude.Text,
    attributeType :: InstanceAttributeType,
    value :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateInstanceAttribute' with the minimum fields required to make a request.
--
-- * 'attributeType' - The type of attribute.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'value' - The value for the attribute. Maximum character limit is 100.
mkUpdateInstanceAttribute ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'attributeType'
  InstanceAttributeType ->
  -- | 'value'
  Lude.Text ->
  UpdateInstanceAttribute
mkUpdateInstanceAttribute pInstanceId_ pAttributeType_ pValue_ =
  UpdateInstanceAttribute'
    { instanceId = pInstanceId_,
      attributeType = pAttributeType_,
      value = pValue_
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiaInstanceId :: Lens.Lens' UpdateInstanceAttribute Lude.Text
uiaInstanceId = Lens.lens (instanceId :: UpdateInstanceAttribute -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateInstanceAttribute)
{-# DEPRECATED uiaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The type of attribute.
--
-- /Note:/ Consider using 'attributeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiaAttributeType :: Lens.Lens' UpdateInstanceAttribute InstanceAttributeType
uiaAttributeType = Lens.lens (attributeType :: UpdateInstanceAttribute -> InstanceAttributeType) (\s a -> s {attributeType = a} :: UpdateInstanceAttribute)
{-# DEPRECATED uiaAttributeType "Use generic-lens or generic-optics with 'attributeType' instead." #-}

-- | The value for the attribute. Maximum character limit is 100.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiaValue :: Lens.Lens' UpdateInstanceAttribute Lude.Text
uiaValue = Lens.lens (value :: UpdateInstanceAttribute -> Lude.Text) (\s a -> s {value = a} :: UpdateInstanceAttribute)
{-# DEPRECATED uiaValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.AWSRequest UpdateInstanceAttribute where
  type Rs UpdateInstanceAttribute = UpdateInstanceAttributeResponse
  request = Req.postJSON connectService
  response = Res.receiveNull UpdateInstanceAttributeResponse'

instance Lude.ToHeaders UpdateInstanceAttribute where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateInstanceAttribute where
  toJSON UpdateInstanceAttribute' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Value" Lude..= value)])

instance Lude.ToPath UpdateInstanceAttribute where
  toPath UpdateInstanceAttribute' {..} =
    Lude.mconcat
      [ "/instance/",
        Lude.toBS instanceId,
        "/attribute/",
        Lude.toBS attributeType
      ]

instance Lude.ToQuery UpdateInstanceAttribute where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateInstanceAttributeResponse' smart constructor.
data UpdateInstanceAttributeResponse = UpdateInstanceAttributeResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateInstanceAttributeResponse' with the minimum fields required to make a request.
mkUpdateInstanceAttributeResponse ::
  UpdateInstanceAttributeResponse
mkUpdateInstanceAttributeResponse =
  UpdateInstanceAttributeResponse'
