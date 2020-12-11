{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.PublishSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Publishes a development schema with a major version and a recommended minor version.
module Network.AWS.CloudDirectory.PublishSchema
  ( -- * Creating a request
    PublishSchema (..),
    mkPublishSchema,

    -- ** Request lenses
    psMinorVersion,
    psName,
    psDevelopmentSchemaARN,
    psVersion,

    -- * Destructuring the response
    PublishSchemaResponse (..),
    mkPublishSchemaResponse,

    -- ** Response lenses
    psrsPublishedSchemaARN,
    psrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPublishSchema' smart constructor.
data PublishSchema = PublishSchema'
  { minorVersion ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    developmentSchemaARN :: Lude.Text,
    version :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PublishSchema' with the minimum fields required to make a request.
--
-- * 'developmentSchemaARN' - The Amazon Resource Name (ARN) that is associated with the development schema. For more information, see 'arns' .
-- * 'minorVersion' - The minor version under which the schema will be published. This parameter is recommended. Schemas have both a major and minor version associated with them.
-- * 'name' - The new name under which the schema will be published. If this is not provided, the development schema is considered.
-- * 'version' - The major version under which the schema will be published. Schemas have both a major and minor version associated with them.
mkPublishSchema ::
  -- | 'developmentSchemaARN'
  Lude.Text ->
  -- | 'version'
  Lude.Text ->
  PublishSchema
mkPublishSchema pDevelopmentSchemaARN_ pVersion_ =
  PublishSchema'
    { minorVersion = Lude.Nothing,
      name = Lude.Nothing,
      developmentSchemaARN = pDevelopmentSchemaARN_,
      version = pVersion_
    }

-- | The minor version under which the schema will be published. This parameter is recommended. Schemas have both a major and minor version associated with them.
--
-- /Note:/ Consider using 'minorVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psMinorVersion :: Lens.Lens' PublishSchema (Lude.Maybe Lude.Text)
psMinorVersion = Lens.lens (minorVersion :: PublishSchema -> Lude.Maybe Lude.Text) (\s a -> s {minorVersion = a} :: PublishSchema)
{-# DEPRECATED psMinorVersion "Use generic-lens or generic-optics with 'minorVersion' instead." #-}

-- | The new name under which the schema will be published. If this is not provided, the development schema is considered.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psName :: Lens.Lens' PublishSchema (Lude.Maybe Lude.Text)
psName = Lens.lens (name :: PublishSchema -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: PublishSchema)
{-# DEPRECATED psName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon Resource Name (ARN) that is associated with the development schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'developmentSchemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psDevelopmentSchemaARN :: Lens.Lens' PublishSchema Lude.Text
psDevelopmentSchemaARN = Lens.lens (developmentSchemaARN :: PublishSchema -> Lude.Text) (\s a -> s {developmentSchemaARN = a} :: PublishSchema)
{-# DEPRECATED psDevelopmentSchemaARN "Use generic-lens or generic-optics with 'developmentSchemaARN' instead." #-}

-- | The major version under which the schema will be published. Schemas have both a major and minor version associated with them.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psVersion :: Lens.Lens' PublishSchema Lude.Text
psVersion = Lens.lens (version :: PublishSchema -> Lude.Text) (\s a -> s {version = a} :: PublishSchema)
{-# DEPRECATED psVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.AWSRequest PublishSchema where
  type Rs PublishSchema = PublishSchemaResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          PublishSchemaResponse'
            Lude.<$> (x Lude..?> "PublishedSchemaArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PublishSchema where
  toHeaders PublishSchema' {..} =
    Lude.mconcat
      ["x-amz-data-partition" Lude.=# developmentSchemaARN]

instance Lude.ToJSON PublishSchema where
  toJSON PublishSchema' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MinorVersion" Lude..=) Lude.<$> minorVersion,
            ("Name" Lude..=) Lude.<$> name,
            Lude.Just ("Version" Lude..= version)
          ]
      )

instance Lude.ToPath PublishSchema where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/schema/publish"

instance Lude.ToQuery PublishSchema where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPublishSchemaResponse' smart constructor.
data PublishSchemaResponse = PublishSchemaResponse'
  { publishedSchemaARN ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PublishSchemaResponse' with the minimum fields required to make a request.
--
-- * 'publishedSchemaARN' - The ARN that is associated with the published schema. For more information, see 'arns' .
-- * 'responseStatus' - The response status code.
mkPublishSchemaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PublishSchemaResponse
mkPublishSchemaResponse pResponseStatus_ =
  PublishSchemaResponse'
    { publishedSchemaARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN that is associated with the published schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'publishedSchemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrsPublishedSchemaARN :: Lens.Lens' PublishSchemaResponse (Lude.Maybe Lude.Text)
psrsPublishedSchemaARN = Lens.lens (publishedSchemaARN :: PublishSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {publishedSchemaARN = a} :: PublishSchemaResponse)
{-# DEPRECATED psrsPublishedSchemaARN "Use generic-lens or generic-optics with 'publishedSchemaARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrsResponseStatus :: Lens.Lens' PublishSchemaResponse Lude.Int
psrsResponseStatus = Lens.lens (responseStatus :: PublishSchemaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PublishSchemaResponse)
{-# DEPRECATED psrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
