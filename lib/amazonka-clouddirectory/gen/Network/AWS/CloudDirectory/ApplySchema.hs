{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ApplySchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the input published schema, at the specified version, into the 'Directory' with the same name and version as that of the published schema.
module Network.AWS.CloudDirectory.ApplySchema
  ( -- * Creating a request
    ApplySchema (..),
    mkApplySchema,

    -- ** Request lenses
    asDirectoryARN,
    asPublishedSchemaARN,

    -- * Destructuring the response
    ApplySchemaResponse (..),
    mkApplySchemaResponse,

    -- ** Response lenses
    asrsDirectoryARN,
    asrsAppliedSchemaARN,
    asrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkApplySchema' smart constructor.
data ApplySchema = ApplySchema'
  { -- | The Amazon Resource Name (ARN) that is associated with the 'Directory' into which the schema is copied. For more information, see 'arns' .
    directoryARN :: Lude.Text,
    -- | Published schema Amazon Resource Name (ARN) that needs to be copied. For more information, see 'arns' .
    publishedSchemaARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplySchema' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' into which the schema is copied. For more information, see 'arns' .
-- * 'publishedSchemaARN' - Published schema Amazon Resource Name (ARN) that needs to be copied. For more information, see 'arns' .
mkApplySchema ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'publishedSchemaARN'
  Lude.Text ->
  ApplySchema
mkApplySchema pDirectoryARN_ pPublishedSchemaARN_ =
  ApplySchema'
    { directoryARN = pDirectoryARN_,
      publishedSchemaARN = pPublishedSchemaARN_
    }

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' into which the schema is copied. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asDirectoryARN :: Lens.Lens' ApplySchema Lude.Text
asDirectoryARN = Lens.lens (directoryARN :: ApplySchema -> Lude.Text) (\s a -> s {directoryARN = a} :: ApplySchema)
{-# DEPRECATED asDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | Published schema Amazon Resource Name (ARN) that needs to be copied. For more information, see 'arns' .
--
-- /Note:/ Consider using 'publishedSchemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asPublishedSchemaARN :: Lens.Lens' ApplySchema Lude.Text
asPublishedSchemaARN = Lens.lens (publishedSchemaARN :: ApplySchema -> Lude.Text) (\s a -> s {publishedSchemaARN = a} :: ApplySchema)
{-# DEPRECATED asPublishedSchemaARN "Use generic-lens or generic-optics with 'publishedSchemaARN' instead." #-}

instance Lude.AWSRequest ApplySchema where
  type Rs ApplySchema = ApplySchemaResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ApplySchemaResponse'
            Lude.<$> (x Lude..?> "DirectoryArn")
            Lude.<*> (x Lude..?> "AppliedSchemaArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ApplySchema where
  toHeaders ApplySchema' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON ApplySchema where
  toJSON ApplySchema' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("PublishedSchemaArn" Lude..= publishedSchemaARN)]
      )

instance Lude.ToPath ApplySchema where
  toPath = Lude.const "/amazonclouddirectory/2017-01-11/schema/apply"

instance Lude.ToQuery ApplySchema where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkApplySchemaResponse' smart constructor.
data ApplySchemaResponse = ApplySchemaResponse'
  { -- | The ARN that is associated with the 'Directory' . For more information, see 'arns' .
    directoryARN :: Lude.Maybe Lude.Text,
    -- | The applied schema ARN that is associated with the copied schema in the 'Directory' . You can use this ARN to describe the schema information applied on this directory. For more information, see 'arns' .
    appliedSchemaARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplySchemaResponse' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The ARN that is associated with the 'Directory' . For more information, see 'arns' .
-- * 'appliedSchemaARN' - The applied schema ARN that is associated with the copied schema in the 'Directory' . You can use this ARN to describe the schema information applied on this directory. For more information, see 'arns' .
-- * 'responseStatus' - The response status code.
mkApplySchemaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ApplySchemaResponse
mkApplySchemaResponse pResponseStatus_ =
  ApplySchemaResponse'
    { directoryARN = Lude.Nothing,
      appliedSchemaARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN that is associated with the 'Directory' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrsDirectoryARN :: Lens.Lens' ApplySchemaResponse (Lude.Maybe Lude.Text)
asrsDirectoryARN = Lens.lens (directoryARN :: ApplySchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {directoryARN = a} :: ApplySchemaResponse)
{-# DEPRECATED asrsDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | The applied schema ARN that is associated with the copied schema in the 'Directory' . You can use this ARN to describe the schema information applied on this directory. For more information, see 'arns' .
--
-- /Note:/ Consider using 'appliedSchemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrsAppliedSchemaARN :: Lens.Lens' ApplySchemaResponse (Lude.Maybe Lude.Text)
asrsAppliedSchemaARN = Lens.lens (appliedSchemaARN :: ApplySchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {appliedSchemaARN = a} :: ApplySchemaResponse)
{-# DEPRECATED asrsAppliedSchemaARN "Use generic-lens or generic-optics with 'appliedSchemaARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrsResponseStatus :: Lens.Lens' ApplySchemaResponse Lude.Int
asrsResponseStatus = Lens.lens (responseStatus :: ApplySchemaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ApplySchemaResponse)
{-# DEPRECATED asrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
