{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyImageAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified AMI. You can specify only one attribute at a time. You can use the @Attribute@ parameter to specify the attribute or one of the following parameters: @Description@ , @LaunchPermission@ , or @ProductCode@ .
--
-- AWS Marketplace product codes cannot be modified. Images with an AWS Marketplace product code cannot be made public.
-- To enable the SriovNetSupport enhanced networking attribute of an image, enable SriovNetSupport on an instance and create an AMI from the instance.
module Network.AWS.EC2.ModifyImageAttribute
  ( -- * Creating a request
    ModifyImageAttribute (..),
    mkModifyImageAttribute,

    -- ** Request lenses
    miaAttribute,
    miaUserIds,
    miaUserGroups,
    miaValue,
    miaLaunchPermission,
    miaOperationType,
    miaProductCodes,
    miaDescription,
    miaDryRun,
    miaImageId,

    -- * Destructuring the response
    ModifyImageAttributeResponse (..),
    mkModifyImageAttributeResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for ModifyImageAttribute.
--
-- /See:/ 'mkModifyImageAttribute' smart constructor.
data ModifyImageAttribute = ModifyImageAttribute'
  { attribute ::
      Lude.Maybe Lude.Text,
    userIds :: Lude.Maybe [Lude.Text],
    userGroups :: Lude.Maybe [Lude.Text],
    value :: Lude.Maybe Lude.Text,
    launchPermission ::
      Lude.Maybe LaunchPermissionModifications,
    operationType :: Lude.Maybe OperationType,
    productCodes :: Lude.Maybe [Lude.Text],
    description :: Lude.Maybe AttributeValue,
    dryRun :: Lude.Maybe Lude.Bool,
    imageId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyImageAttribute' with the minimum fields required to make a request.
--
-- * 'attribute' - The name of the attribute to modify. The valid values are @description@ , @launchPermission@ , and @productCodes@ .
-- * 'description' - A new description for the AMI.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'imageId' - The ID of the AMI.
-- * 'launchPermission' - A new launch permission for the AMI.
-- * 'operationType' - The operation type. This parameter can be used only when the @Attribute@ parameter is @launchPermission@ .
-- * 'productCodes' - The DevPay product codes. After you add a product code to an AMI, it can't be removed.
-- * 'userGroups' - The user groups. This parameter can be used only when the @Attribute@ parameter is @launchPermission@ .
-- * 'userIds' - The AWS account IDs. This parameter can be used only when the @Attribute@ parameter is @launchPermission@ .
-- * 'value' - The value of the attribute being modified. This parameter can be used only when the @Attribute@ parameter is @description@ or @productCodes@ .
mkModifyImageAttribute ::
  -- | 'imageId'
  Lude.Text ->
  ModifyImageAttribute
mkModifyImageAttribute pImageId_ =
  ModifyImageAttribute'
    { attribute = Lude.Nothing,
      userIds = Lude.Nothing,
      userGroups = Lude.Nothing,
      value = Lude.Nothing,
      launchPermission = Lude.Nothing,
      operationType = Lude.Nothing,
      productCodes = Lude.Nothing,
      description = Lude.Nothing,
      dryRun = Lude.Nothing,
      imageId = pImageId_
    }

-- | The name of the attribute to modify. The valid values are @description@ , @launchPermission@ , and @productCodes@ .
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaAttribute :: Lens.Lens' ModifyImageAttribute (Lude.Maybe Lude.Text)
miaAttribute = Lens.lens (attribute :: ModifyImageAttribute -> Lude.Maybe Lude.Text) (\s a -> s {attribute = a} :: ModifyImageAttribute)
{-# DEPRECATED miaAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The AWS account IDs. This parameter can be used only when the @Attribute@ parameter is @launchPermission@ .
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaUserIds :: Lens.Lens' ModifyImageAttribute (Lude.Maybe [Lude.Text])
miaUserIds = Lens.lens (userIds :: ModifyImageAttribute -> Lude.Maybe [Lude.Text]) (\s a -> s {userIds = a} :: ModifyImageAttribute)
{-# DEPRECATED miaUserIds "Use generic-lens or generic-optics with 'userIds' instead." #-}

-- | The user groups. This parameter can be used only when the @Attribute@ parameter is @launchPermission@ .
--
-- /Note:/ Consider using 'userGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaUserGroups :: Lens.Lens' ModifyImageAttribute (Lude.Maybe [Lude.Text])
miaUserGroups = Lens.lens (userGroups :: ModifyImageAttribute -> Lude.Maybe [Lude.Text]) (\s a -> s {userGroups = a} :: ModifyImageAttribute)
{-# DEPRECATED miaUserGroups "Use generic-lens or generic-optics with 'userGroups' instead." #-}

-- | The value of the attribute being modified. This parameter can be used only when the @Attribute@ parameter is @description@ or @productCodes@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaValue :: Lens.Lens' ModifyImageAttribute (Lude.Maybe Lude.Text)
miaValue = Lens.lens (value :: ModifyImageAttribute -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: ModifyImageAttribute)
{-# DEPRECATED miaValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | A new launch permission for the AMI.
--
-- /Note:/ Consider using 'launchPermission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaLaunchPermission :: Lens.Lens' ModifyImageAttribute (Lude.Maybe LaunchPermissionModifications)
miaLaunchPermission = Lens.lens (launchPermission :: ModifyImageAttribute -> Lude.Maybe LaunchPermissionModifications) (\s a -> s {launchPermission = a} :: ModifyImageAttribute)
{-# DEPRECATED miaLaunchPermission "Use generic-lens or generic-optics with 'launchPermission' instead." #-}

-- | The operation type. This parameter can be used only when the @Attribute@ parameter is @launchPermission@ .
--
-- /Note:/ Consider using 'operationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaOperationType :: Lens.Lens' ModifyImageAttribute (Lude.Maybe OperationType)
miaOperationType = Lens.lens (operationType :: ModifyImageAttribute -> Lude.Maybe OperationType) (\s a -> s {operationType = a} :: ModifyImageAttribute)
{-# DEPRECATED miaOperationType "Use generic-lens or generic-optics with 'operationType' instead." #-}

-- | The DevPay product codes. After you add a product code to an AMI, it can't be removed.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaProductCodes :: Lens.Lens' ModifyImageAttribute (Lude.Maybe [Lude.Text])
miaProductCodes = Lens.lens (productCodes :: ModifyImageAttribute -> Lude.Maybe [Lude.Text]) (\s a -> s {productCodes = a} :: ModifyImageAttribute)
{-# DEPRECATED miaProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | A new description for the AMI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaDescription :: Lens.Lens' ModifyImageAttribute (Lude.Maybe AttributeValue)
miaDescription = Lens.lens (description :: ModifyImageAttribute -> Lude.Maybe AttributeValue) (\s a -> s {description = a} :: ModifyImageAttribute)
{-# DEPRECATED miaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaDryRun :: Lens.Lens' ModifyImageAttribute (Lude.Maybe Lude.Bool)
miaDryRun = Lens.lens (dryRun :: ModifyImageAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyImageAttribute)
{-# DEPRECATED miaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miaImageId :: Lens.Lens' ModifyImageAttribute Lude.Text
miaImageId = Lens.lens (imageId :: ModifyImageAttribute -> Lude.Text) (\s a -> s {imageId = a} :: ModifyImageAttribute)
{-# DEPRECATED miaImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

instance Lude.AWSRequest ModifyImageAttribute where
  type Rs ModifyImageAttribute = ModifyImageAttributeResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull ModifyImageAttributeResponse'

instance Lude.ToHeaders ModifyImageAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyImageAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyImageAttribute where
  toQuery ModifyImageAttribute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyImageAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Attribute" Lude.=: attribute,
        Lude.toQuery (Lude.toQueryList "UserId" Lude.<$> userIds),
        Lude.toQuery (Lude.toQueryList "UserGroup" Lude.<$> userGroups),
        "Value" Lude.=: value,
        "LaunchPermission" Lude.=: launchPermission,
        "OperationType" Lude.=: operationType,
        Lude.toQuery
          (Lude.toQueryList "ProductCode" Lude.<$> productCodes),
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun,
        "ImageId" Lude.=: imageId
      ]

-- | /See:/ 'mkModifyImageAttributeResponse' smart constructor.
data ModifyImageAttributeResponse = ModifyImageAttributeResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyImageAttributeResponse' with the minimum fields required to make a request.
mkModifyImageAttributeResponse ::
  ModifyImageAttributeResponse
mkModifyImageAttributeResponse = ModifyImageAttributeResponse'
