{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DeregisterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a type or type version from active use in the CloudFormation registry. If a type or type version is deregistered, it cannot be used in CloudFormation operations.
--
-- To deregister a type, you must individually deregister all registered versions of that type. If a type has only a single registered version, deregistering that version results in the type itself being deregistered.
-- You cannot deregister the default version of a type, unless it is the only registered version of that type, in which case the type itself is deregistered as well.
module Network.AWS.CloudFormation.DeregisterType
  ( -- * Creating a request
    DeregisterType (..),
    mkDeregisterType,

    -- ** Request lenses
    dVersionId,
    dTypeName,
    dARN,
    dType,

    -- * Destructuring the response
    DeregisterTypeResponse (..),
    mkDeregisterTypeResponse,

    -- ** Response lenses
    dtrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeregisterType' smart constructor.
data DeregisterType = DeregisterType'
  { versionId ::
      Lude.Maybe Lude.Text,
    typeName :: Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe RegistryType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterType' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
-- * 'type'' - The kind of type.
--
-- Currently the only valid value is @RESOURCE@ .
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
-- * 'typeName' - The name of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
-- * 'versionId' - The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
mkDeregisterType ::
  DeregisterType
mkDeregisterType =
  DeregisterType'
    { versionId = Lude.Nothing,
      typeName = Lude.Nothing,
      arn = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVersionId :: Lens.Lens' DeregisterType (Lude.Maybe Lude.Text)
dVersionId = Lens.lens (versionId :: DeregisterType -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: DeregisterType)
{-# DEPRECATED dVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The name of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTypeName :: Lens.Lens' DeregisterType (Lude.Maybe Lude.Text)
dTypeName = Lens.lens (typeName :: DeregisterType -> Lude.Maybe Lude.Text) (\s a -> s {typeName = a} :: DeregisterType)
{-# DEPRECATED dTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The Amazon Resource Name (ARN) of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dARN :: Lens.Lens' DeregisterType (Lude.Maybe Lude.Text)
dARN = Lens.lens (arn :: DeregisterType -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DeregisterType)
{-# DEPRECATED dARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The kind of type.
--
-- Currently the only valid value is @RESOURCE@ .
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dType :: Lens.Lens' DeregisterType (Lude.Maybe RegistryType)
dType = Lens.lens (type' :: DeregisterType -> Lude.Maybe RegistryType) (\s a -> s {type' = a} :: DeregisterType)
{-# DEPRECATED dType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.AWSRequest DeregisterType where
  type Rs DeregisterType = DeregisterTypeResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DeregisterTypeResult"
      ( \s h x ->
          DeregisterTypeResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeregisterType where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeregisterType where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterType where
  toQuery DeregisterType' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeregisterType" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "VersionId" Lude.=: versionId,
        "TypeName" Lude.=: typeName,
        "Arn" Lude.=: arn,
        "Type" Lude.=: type'
      ]

-- | /See:/ 'mkDeregisterTypeResponse' smart constructor.
newtype DeregisterTypeResponse = DeregisterTypeResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterTypeResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeregisterTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeregisterTypeResponse
mkDeregisterTypeResponse pResponseStatus_ =
  DeregisterTypeResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DeregisterTypeResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DeregisterTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeregisterTypeResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
