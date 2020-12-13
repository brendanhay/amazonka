{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.CreateDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'Directory' by copying the published schema into the directory. A directory cannot be created without a schema.
--
-- You can also quickly create a directory using a managed schema, called the @QuickStartSchema@ . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_managed.html Managed Schema> in the /Amazon Cloud Directory Developer Guide/ .
module Network.AWS.CloudDirectory.CreateDirectory
  ( -- * Creating a request
    CreateDirectory (..),
    mkCreateDirectory,

    -- ** Request lenses
    cdSchemaARN,
    cdName,

    -- * Destructuring the response
    CreateDirectoryResponse (..),
    mkCreateDirectoryResponse,

    -- ** Response lenses
    cdrsDirectoryARN,
    cdrsObjectIdentifier,
    cdrsAppliedSchemaARN,
    cdrsName,
    cdrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDirectory' smart constructor.
data CreateDirectory = CreateDirectory'
  { -- | The Amazon Resource Name (ARN) of the published schema that will be copied into the data 'Directory' . For more information, see 'arns' .
    schemaARN :: Lude.Text,
    -- | The name of the 'Directory' . Should be unique per account, per region.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDirectory' with the minimum fields required to make a request.
--
-- * 'schemaARN' - The Amazon Resource Name (ARN) of the published schema that will be copied into the data 'Directory' . For more information, see 'arns' .
-- * 'name' - The name of the 'Directory' . Should be unique per account, per region.
mkCreateDirectory ::
  -- | 'schemaARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  CreateDirectory
mkCreateDirectory pSchemaARN_ pName_ =
  CreateDirectory' {schemaARN = pSchemaARN_, name = pName_}

-- | The Amazon Resource Name (ARN) of the published schema that will be copied into the data 'Directory' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSchemaARN :: Lens.Lens' CreateDirectory Lude.Text
cdSchemaARN = Lens.lens (schemaARN :: CreateDirectory -> Lude.Text) (\s a -> s {schemaARN = a} :: CreateDirectory)
{-# DEPRECATED cdSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The name of the 'Directory' . Should be unique per account, per region.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdName :: Lens.Lens' CreateDirectory Lude.Text
cdName = Lens.lens (name :: CreateDirectory -> Lude.Text) (\s a -> s {name = a} :: CreateDirectory)
{-# DEPRECATED cdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateDirectory where
  type Rs CreateDirectory = CreateDirectoryResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDirectoryResponse'
            Lude.<$> (x Lude..:> "DirectoryArn")
            Lude.<*> (x Lude..:> "ObjectIdentifier")
            Lude.<*> (x Lude..:> "AppliedSchemaArn")
            Lude.<*> (x Lude..:> "Name")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDirectory where
  toHeaders CreateDirectory' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# schemaARN]

instance Lude.ToJSON CreateDirectory where
  toJSON CreateDirectory' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath CreateDirectory where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/directory/create"

instance Lude.ToQuery CreateDirectory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDirectoryResponse' smart constructor.
data CreateDirectoryResponse = CreateDirectoryResponse'
  { -- | The ARN that is associated with the 'Directory' . For more information, see 'arns' .
    directoryARN :: Lude.Text,
    -- | The root object node of the created directory.
    objectIdentifier :: Lude.Text,
    -- | The ARN of the published schema in the 'Directory' . Once a published schema is copied into the directory, it has its own ARN, which is referred to applied schema ARN. For more information, see 'arns' .
    appliedSchemaARN :: Lude.Text,
    -- | The name of the 'Directory' .
    name :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDirectoryResponse' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The ARN that is associated with the 'Directory' . For more information, see 'arns' .
-- * 'objectIdentifier' - The root object node of the created directory.
-- * 'appliedSchemaARN' - The ARN of the published schema in the 'Directory' . Once a published schema is copied into the directory, it has its own ARN, which is referred to applied schema ARN. For more information, see 'arns' .
-- * 'name' - The name of the 'Directory' .
-- * 'responseStatus' - The response status code.
mkCreateDirectoryResponse ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'objectIdentifier'
  Lude.Text ->
  -- | 'appliedSchemaARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateDirectoryResponse
mkCreateDirectoryResponse
  pDirectoryARN_
  pObjectIdentifier_
  pAppliedSchemaARN_
  pName_
  pResponseStatus_ =
    CreateDirectoryResponse'
      { directoryARN = pDirectoryARN_,
        objectIdentifier = pObjectIdentifier_,
        appliedSchemaARN = pAppliedSchemaARN_,
        name = pName_,
        responseStatus = pResponseStatus_
      }

-- | The ARN that is associated with the 'Directory' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsDirectoryARN :: Lens.Lens' CreateDirectoryResponse Lude.Text
cdrsDirectoryARN = Lens.lens (directoryARN :: CreateDirectoryResponse -> Lude.Text) (\s a -> s {directoryARN = a} :: CreateDirectoryResponse)
{-# DEPRECATED cdrsDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | The root object node of the created directory.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsObjectIdentifier :: Lens.Lens' CreateDirectoryResponse Lude.Text
cdrsObjectIdentifier = Lens.lens (objectIdentifier :: CreateDirectoryResponse -> Lude.Text) (\s a -> s {objectIdentifier = a} :: CreateDirectoryResponse)
{-# DEPRECATED cdrsObjectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead." #-}

-- | The ARN of the published schema in the 'Directory' . Once a published schema is copied into the directory, it has its own ARN, which is referred to applied schema ARN. For more information, see 'arns' .
--
-- /Note:/ Consider using 'appliedSchemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsAppliedSchemaARN :: Lens.Lens' CreateDirectoryResponse Lude.Text
cdrsAppliedSchemaARN = Lens.lens (appliedSchemaARN :: CreateDirectoryResponse -> Lude.Text) (\s a -> s {appliedSchemaARN = a} :: CreateDirectoryResponse)
{-# DEPRECATED cdrsAppliedSchemaARN "Use generic-lens or generic-optics with 'appliedSchemaARN' instead." #-}

-- | The name of the 'Directory' .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsName :: Lens.Lens' CreateDirectoryResponse Lude.Text
cdrsName = Lens.lens (name :: CreateDirectoryResponse -> Lude.Text) (\s a -> s {name = a} :: CreateDirectoryResponse)
{-# DEPRECATED cdrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsResponseStatus :: Lens.Lens' CreateDirectoryResponse Lude.Int
cdrsResponseStatus = Lens.lens (responseStatus :: CreateDirectoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDirectoryResponse)
{-# DEPRECATED cdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
