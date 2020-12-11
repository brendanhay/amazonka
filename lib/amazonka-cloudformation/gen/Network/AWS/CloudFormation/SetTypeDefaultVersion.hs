{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.SetTypeDefaultVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specify the default version of a type. The default version of a type will be used in CloudFormation operations.
module Network.AWS.CloudFormation.SetTypeDefaultVersion
  ( -- * Creating a request
    SetTypeDefaultVersion (..),
    mkSetTypeDefaultVersion,

    -- ** Request lenses
    stdvVersionId,
    stdvTypeName,
    stdvARN,
    stdvType,

    -- * Destructuring the response
    SetTypeDefaultVersionResponse (..),
    mkSetTypeDefaultVersionResponse,

    -- ** Response lenses
    stdvrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetTypeDefaultVersion' smart constructor.
data SetTypeDefaultVersion = SetTypeDefaultVersion'
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

-- | Creates a value of 'SetTypeDefaultVersion' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the type for which you want version summary information.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
-- * 'type'' - The kind of type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
-- * 'typeName' - The name of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
-- * 'versionId' - The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
mkSetTypeDefaultVersion ::
  SetTypeDefaultVersion
mkSetTypeDefaultVersion =
  SetTypeDefaultVersion'
    { versionId = Lude.Nothing,
      typeName = Lude.Nothing,
      arn = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdvVersionId :: Lens.Lens' SetTypeDefaultVersion (Lude.Maybe Lude.Text)
stdvVersionId = Lens.lens (versionId :: SetTypeDefaultVersion -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: SetTypeDefaultVersion)
{-# DEPRECATED stdvVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The name of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdvTypeName :: Lens.Lens' SetTypeDefaultVersion (Lude.Maybe Lude.Text)
stdvTypeName = Lens.lens (typeName :: SetTypeDefaultVersion -> Lude.Maybe Lude.Text) (\s a -> s {typeName = a} :: SetTypeDefaultVersion)
{-# DEPRECATED stdvTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The Amazon Resource Name (ARN) of the type for which you want version summary information.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdvARN :: Lens.Lens' SetTypeDefaultVersion (Lude.Maybe Lude.Text)
stdvARN = Lens.lens (arn :: SetTypeDefaultVersion -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: SetTypeDefaultVersion)
{-# DEPRECATED stdvARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The kind of type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdvType :: Lens.Lens' SetTypeDefaultVersion (Lude.Maybe RegistryType)
stdvType = Lens.lens (type' :: SetTypeDefaultVersion -> Lude.Maybe RegistryType) (\s a -> s {type' = a} :: SetTypeDefaultVersion)
{-# DEPRECATED stdvType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.AWSRequest SetTypeDefaultVersion where
  type Rs SetTypeDefaultVersion = SetTypeDefaultVersionResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "SetTypeDefaultVersionResult"
      ( \s h x ->
          SetTypeDefaultVersionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetTypeDefaultVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetTypeDefaultVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery SetTypeDefaultVersion where
  toQuery SetTypeDefaultVersion' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SetTypeDefaultVersion" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "VersionId" Lude.=: versionId,
        "TypeName" Lude.=: typeName,
        "Arn" Lude.=: arn,
        "Type" Lude.=: type'
      ]

-- | /See:/ 'mkSetTypeDefaultVersionResponse' smart constructor.
newtype SetTypeDefaultVersionResponse = SetTypeDefaultVersionResponse'
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

-- | Creates a value of 'SetTypeDefaultVersionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSetTypeDefaultVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetTypeDefaultVersionResponse
mkSetTypeDefaultVersionResponse pResponseStatus_ =
  SetTypeDefaultVersionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdvrsResponseStatus :: Lens.Lens' SetTypeDefaultVersionResponse Lude.Int
stdvrsResponseStatus = Lens.lens (responseStatus :: SetTypeDefaultVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetTypeDefaultVersionResponse)
{-# DEPRECATED stdvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
