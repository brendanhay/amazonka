{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyFpgaImageAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified Amazon FPGA Image (AFI).
module Network.AWS.EC2.ModifyFpgaImageAttribute
  ( -- * Creating a request
    ModifyFpgaImageAttribute (..),
    mkModifyFpgaImageAttribute,

    -- ** Request lenses
    mfiaAttribute,
    mfiaUserIds,
    mfiaFpgaImageId,
    mfiaUserGroups,
    mfiaLoadPermission,
    mfiaName,
    mfiaOperationType,
    mfiaProductCodes,
    mfiaDescription,
    mfiaDryRun,

    -- * Destructuring the response
    ModifyFpgaImageAttributeResponse (..),
    mkModifyFpgaImageAttributeResponse,

    -- ** Response lenses
    mfiarsFpgaImageAttribute,
    mfiarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyFpgaImageAttribute' smart constructor.
data ModifyFpgaImageAttribute = ModifyFpgaImageAttribute'
  { -- | The name of the attribute.
    attribute :: Lude.Maybe FpgaImageAttributeName,
    -- | The AWS account IDs. This parameter is valid only when modifying the @loadPermission@ attribute.
    userIds :: Lude.Maybe [Lude.Text],
    -- | The ID of the AFI.
    fpgaImageId :: Lude.Text,
    -- | The user groups. This parameter is valid only when modifying the @loadPermission@ attribute.
    userGroups :: Lude.Maybe [Lude.Text],
    -- | The load permission for the AFI.
    loadPermission :: Lude.Maybe LoadPermissionModifications,
    -- | A name for the AFI.
    name :: Lude.Maybe Lude.Text,
    -- | The operation type.
    operationType :: Lude.Maybe OperationType,
    -- | The product codes. After you add a product code to an AFI, it can't be removed. This parameter is valid only when modifying the @productCodes@ attribute.
    productCodes :: Lude.Maybe [Lude.Text],
    -- | A description for the AFI.
    description :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyFpgaImageAttribute' with the minimum fields required to make a request.
--
-- * 'attribute' - The name of the attribute.
-- * 'userIds' - The AWS account IDs. This parameter is valid only when modifying the @loadPermission@ attribute.
-- * 'fpgaImageId' - The ID of the AFI.
-- * 'userGroups' - The user groups. This parameter is valid only when modifying the @loadPermission@ attribute.
-- * 'loadPermission' - The load permission for the AFI.
-- * 'name' - A name for the AFI.
-- * 'operationType' - The operation type.
-- * 'productCodes' - The product codes. After you add a product code to an AFI, it can't be removed. This parameter is valid only when modifying the @productCodes@ attribute.
-- * 'description' - A description for the AFI.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkModifyFpgaImageAttribute ::
  -- | 'fpgaImageId'
  Lude.Text ->
  ModifyFpgaImageAttribute
mkModifyFpgaImageAttribute pFpgaImageId_ =
  ModifyFpgaImageAttribute'
    { attribute = Lude.Nothing,
      userIds = Lude.Nothing,
      fpgaImageId = pFpgaImageId_,
      userGroups = Lude.Nothing,
      loadPermission = Lude.Nothing,
      name = Lude.Nothing,
      operationType = Lude.Nothing,
      productCodes = Lude.Nothing,
      description = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The name of the attribute.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaAttribute :: Lens.Lens' ModifyFpgaImageAttribute (Lude.Maybe FpgaImageAttributeName)
mfiaAttribute = Lens.lens (attribute :: ModifyFpgaImageAttribute -> Lude.Maybe FpgaImageAttributeName) (\s a -> s {attribute = a} :: ModifyFpgaImageAttribute)
{-# DEPRECATED mfiaAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The AWS account IDs. This parameter is valid only when modifying the @loadPermission@ attribute.
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaUserIds :: Lens.Lens' ModifyFpgaImageAttribute (Lude.Maybe [Lude.Text])
mfiaUserIds = Lens.lens (userIds :: ModifyFpgaImageAttribute -> Lude.Maybe [Lude.Text]) (\s a -> s {userIds = a} :: ModifyFpgaImageAttribute)
{-# DEPRECATED mfiaUserIds "Use generic-lens or generic-optics with 'userIds' instead." #-}

-- | The ID of the AFI.
--
-- /Note:/ Consider using 'fpgaImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaFpgaImageId :: Lens.Lens' ModifyFpgaImageAttribute Lude.Text
mfiaFpgaImageId = Lens.lens (fpgaImageId :: ModifyFpgaImageAttribute -> Lude.Text) (\s a -> s {fpgaImageId = a} :: ModifyFpgaImageAttribute)
{-# DEPRECATED mfiaFpgaImageId "Use generic-lens or generic-optics with 'fpgaImageId' instead." #-}

-- | The user groups. This parameter is valid only when modifying the @loadPermission@ attribute.
--
-- /Note:/ Consider using 'userGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaUserGroups :: Lens.Lens' ModifyFpgaImageAttribute (Lude.Maybe [Lude.Text])
mfiaUserGroups = Lens.lens (userGroups :: ModifyFpgaImageAttribute -> Lude.Maybe [Lude.Text]) (\s a -> s {userGroups = a} :: ModifyFpgaImageAttribute)
{-# DEPRECATED mfiaUserGroups "Use generic-lens or generic-optics with 'userGroups' instead." #-}

-- | The load permission for the AFI.
--
-- /Note:/ Consider using 'loadPermission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaLoadPermission :: Lens.Lens' ModifyFpgaImageAttribute (Lude.Maybe LoadPermissionModifications)
mfiaLoadPermission = Lens.lens (loadPermission :: ModifyFpgaImageAttribute -> Lude.Maybe LoadPermissionModifications) (\s a -> s {loadPermission = a} :: ModifyFpgaImageAttribute)
{-# DEPRECATED mfiaLoadPermission "Use generic-lens or generic-optics with 'loadPermission' instead." #-}

-- | A name for the AFI.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaName :: Lens.Lens' ModifyFpgaImageAttribute (Lude.Maybe Lude.Text)
mfiaName = Lens.lens (name :: ModifyFpgaImageAttribute -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ModifyFpgaImageAttribute)
{-# DEPRECATED mfiaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The operation type.
--
-- /Note:/ Consider using 'operationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaOperationType :: Lens.Lens' ModifyFpgaImageAttribute (Lude.Maybe OperationType)
mfiaOperationType = Lens.lens (operationType :: ModifyFpgaImageAttribute -> Lude.Maybe OperationType) (\s a -> s {operationType = a} :: ModifyFpgaImageAttribute)
{-# DEPRECATED mfiaOperationType "Use generic-lens or generic-optics with 'operationType' instead." #-}

-- | The product codes. After you add a product code to an AFI, it can't be removed. This parameter is valid only when modifying the @productCodes@ attribute.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaProductCodes :: Lens.Lens' ModifyFpgaImageAttribute (Lude.Maybe [Lude.Text])
mfiaProductCodes = Lens.lens (productCodes :: ModifyFpgaImageAttribute -> Lude.Maybe [Lude.Text]) (\s a -> s {productCodes = a} :: ModifyFpgaImageAttribute)
{-# DEPRECATED mfiaProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | A description for the AFI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaDescription :: Lens.Lens' ModifyFpgaImageAttribute (Lude.Maybe Lude.Text)
mfiaDescription = Lens.lens (description :: ModifyFpgaImageAttribute -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ModifyFpgaImageAttribute)
{-# DEPRECATED mfiaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiaDryRun :: Lens.Lens' ModifyFpgaImageAttribute (Lude.Maybe Lude.Bool)
mfiaDryRun = Lens.lens (dryRun :: ModifyFpgaImageAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyFpgaImageAttribute)
{-# DEPRECATED mfiaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ModifyFpgaImageAttribute where
  type Rs ModifyFpgaImageAttribute = ModifyFpgaImageAttributeResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyFpgaImageAttributeResponse'
            Lude.<$> (x Lude..@? "fpgaImageAttribute")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyFpgaImageAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyFpgaImageAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyFpgaImageAttribute where
  toQuery ModifyFpgaImageAttribute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyFpgaImageAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Attribute" Lude.=: attribute,
        Lude.toQuery (Lude.toQueryList "UserId" Lude.<$> userIds),
        "FpgaImageId" Lude.=: fpgaImageId,
        Lude.toQuery (Lude.toQueryList "UserGroup" Lude.<$> userGroups),
        "LoadPermission" Lude.=: loadPermission,
        "Name" Lude.=: name,
        "OperationType" Lude.=: operationType,
        Lude.toQuery
          (Lude.toQueryList "ProductCode" Lude.<$> productCodes),
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkModifyFpgaImageAttributeResponse' smart constructor.
data ModifyFpgaImageAttributeResponse = ModifyFpgaImageAttributeResponse'
  { -- | Information about the attribute.
    fpgaImageAttribute :: Lude.Maybe FpgaImageAttribute,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyFpgaImageAttributeResponse' with the minimum fields required to make a request.
--
-- * 'fpgaImageAttribute' - Information about the attribute.
-- * 'responseStatus' - The response status code.
mkModifyFpgaImageAttributeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyFpgaImageAttributeResponse
mkModifyFpgaImageAttributeResponse pResponseStatus_ =
  ModifyFpgaImageAttributeResponse'
    { fpgaImageAttribute =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the attribute.
--
-- /Note:/ Consider using 'fpgaImageAttribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiarsFpgaImageAttribute :: Lens.Lens' ModifyFpgaImageAttributeResponse (Lude.Maybe FpgaImageAttribute)
mfiarsFpgaImageAttribute = Lens.lens (fpgaImageAttribute :: ModifyFpgaImageAttributeResponse -> Lude.Maybe FpgaImageAttribute) (\s a -> s {fpgaImageAttribute = a} :: ModifyFpgaImageAttributeResponse)
{-# DEPRECATED mfiarsFpgaImageAttribute "Use generic-lens or generic-optics with 'fpgaImageAttribute' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfiarsResponseStatus :: Lens.Lens' ModifyFpgaImageAttributeResponse Lude.Int
mfiarsResponseStatus = Lens.lens (responseStatus :: ModifyFpgaImageAttributeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyFpgaImageAttributeResponse)
{-# DEPRECATED mfiarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
