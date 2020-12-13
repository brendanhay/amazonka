{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.PublishLayerVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> from a ZIP archive. Each time you call @PublishLayerVersion@ with the same layer name, a new version is created.
--
-- Add layers to your function with 'CreateFunction' or 'UpdateFunctionConfiguration' .
module Network.AWS.Lambda.PublishLayerVersion
  ( -- * Creating a request
    PublishLayerVersion (..),
    mkPublishLayerVersion,

    -- ** Request lenses
    plvLayerName,
    plvContent,
    plvLicenseInfo,
    plvDescription,
    plvCompatibleRuntimes,

    -- * Destructuring the response
    PublishLayerVersionResponse (..),
    mkPublishLayerVersionResponse,

    -- ** Response lenses
    plvrsLayerVersionARN,
    plvrsContent,
    plvrsCreatedDate,
    plvrsVersion,
    plvrsLicenseInfo,
    plvrsLayerARN,
    plvrsDescription,
    plvrsCompatibleRuntimes,
    plvrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPublishLayerVersion' smart constructor.
data PublishLayerVersion = PublishLayerVersion'
  { -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Lude.Text,
    -- | The function layer archive.
    content :: LayerVersionContentInput,
    -- | The layer's software license. It can be any of the following:
    --
    --
    --     * An <https://spdx.org/licenses/ SPDX license identifier> . For example, @MIT@ .
    --
    --
    --     * The URL of a license hosted on the internet. For example, @https://opensource.org/licenses/MIT@ .
    --
    --
    --     * The full text of the license.
    licenseInfo :: Lude.Maybe Lude.Text,
    -- | The description of the version.
    description :: Lude.Maybe Lude.Text,
    -- | A list of compatible <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html function runtimes> . Used for filtering with 'ListLayers' and 'ListLayerVersions' .
    compatibleRuntimes :: Lude.Maybe [Runtime]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PublishLayerVersion' with the minimum fields required to make a request.
--
-- * 'layerName' - The name or Amazon Resource Name (ARN) of the layer.
-- * 'content' - The function layer archive.
-- * 'licenseInfo' - The layer's software license. It can be any of the following:
--
--
--     * An <https://spdx.org/licenses/ SPDX license identifier> . For example, @MIT@ .
--
--
--     * The URL of a license hosted on the internet. For example, @https://opensource.org/licenses/MIT@ .
--
--
--     * The full text of the license.
--
--
-- * 'description' - The description of the version.
-- * 'compatibleRuntimes' - A list of compatible <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html function runtimes> . Used for filtering with 'ListLayers' and 'ListLayerVersions' .
mkPublishLayerVersion ::
  -- | 'layerName'
  Lude.Text ->
  -- | 'content'
  LayerVersionContentInput ->
  PublishLayerVersion
mkPublishLayerVersion pLayerName_ pContent_ =
  PublishLayerVersion'
    { layerName = pLayerName_,
      content = pContent_,
      licenseInfo = Lude.Nothing,
      description = Lude.Nothing,
      compatibleRuntimes = Lude.Nothing
    }

-- | The name or Amazon Resource Name (ARN) of the layer.
--
-- /Note:/ Consider using 'layerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvLayerName :: Lens.Lens' PublishLayerVersion Lude.Text
plvLayerName = Lens.lens (layerName :: PublishLayerVersion -> Lude.Text) (\s a -> s {layerName = a} :: PublishLayerVersion)
{-# DEPRECATED plvLayerName "Use generic-lens or generic-optics with 'layerName' instead." #-}

-- | The function layer archive.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvContent :: Lens.Lens' PublishLayerVersion LayerVersionContentInput
plvContent = Lens.lens (content :: PublishLayerVersion -> LayerVersionContentInput) (\s a -> s {content = a} :: PublishLayerVersion)
{-# DEPRECATED plvContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The layer's software license. It can be any of the following:
--
--
--     * An <https://spdx.org/licenses/ SPDX license identifier> . For example, @MIT@ .
--
--
--     * The URL of a license hosted on the internet. For example, @https://opensource.org/licenses/MIT@ .
--
--
--     * The full text of the license.
--
--
--
-- /Note:/ Consider using 'licenseInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvLicenseInfo :: Lens.Lens' PublishLayerVersion (Lude.Maybe Lude.Text)
plvLicenseInfo = Lens.lens (licenseInfo :: PublishLayerVersion -> Lude.Maybe Lude.Text) (\s a -> s {licenseInfo = a} :: PublishLayerVersion)
{-# DEPRECATED plvLicenseInfo "Use generic-lens or generic-optics with 'licenseInfo' instead." #-}

-- | The description of the version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvDescription :: Lens.Lens' PublishLayerVersion (Lude.Maybe Lude.Text)
plvDescription = Lens.lens (description :: PublishLayerVersion -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PublishLayerVersion)
{-# DEPRECATED plvDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of compatible <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html function runtimes> . Used for filtering with 'ListLayers' and 'ListLayerVersions' .
--
-- /Note:/ Consider using 'compatibleRuntimes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvCompatibleRuntimes :: Lens.Lens' PublishLayerVersion (Lude.Maybe [Runtime])
plvCompatibleRuntimes = Lens.lens (compatibleRuntimes :: PublishLayerVersion -> Lude.Maybe [Runtime]) (\s a -> s {compatibleRuntimes = a} :: PublishLayerVersion)
{-# DEPRECATED plvCompatibleRuntimes "Use generic-lens or generic-optics with 'compatibleRuntimes' instead." #-}

instance Lude.AWSRequest PublishLayerVersion where
  type Rs PublishLayerVersion = PublishLayerVersionResponse
  request = Req.postJSON lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          PublishLayerVersionResponse'
            Lude.<$> (x Lude..?> "LayerVersionArn")
            Lude.<*> (x Lude..?> "Content")
            Lude.<*> (x Lude..?> "CreatedDate")
            Lude.<*> (x Lude..?> "Version")
            Lude.<*> (x Lude..?> "LicenseInfo")
            Lude.<*> (x Lude..?> "LayerArn")
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (x Lude..?> "CompatibleRuntimes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PublishLayerVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON PublishLayerVersion where
  toJSON PublishLayerVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Content" Lude..= content),
            ("LicenseInfo" Lude..=) Lude.<$> licenseInfo,
            ("Description" Lude..=) Lude.<$> description,
            ("CompatibleRuntimes" Lude..=) Lude.<$> compatibleRuntimes
          ]
      )

instance Lude.ToPath PublishLayerVersion where
  toPath PublishLayerVersion' {..} =
    Lude.mconcat
      ["/2018-10-31/layers/", Lude.toBS layerName, "/versions"]

instance Lude.ToQuery PublishLayerVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPublishLayerVersionResponse' smart constructor.
data PublishLayerVersionResponse = PublishLayerVersionResponse'
  { -- | The ARN of the layer version.
    layerVersionARN :: Lude.Maybe Lude.Text,
    -- | Details about the layer version.
    content :: Lude.Maybe LayerVersionContentOutput,
    -- | The date that the layer version was created, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
    createdDate :: Lude.Maybe Lude.Text,
    -- | The version number.
    version :: Lude.Maybe Lude.Integer,
    -- | The layer's software license.
    licenseInfo :: Lude.Maybe Lude.Text,
    -- | The ARN of the layer.
    layerARN :: Lude.Maybe Lude.Text,
    -- | The description of the version.
    description :: Lude.Maybe Lude.Text,
    -- | The layer's compatible runtimes.
    compatibleRuntimes :: Lude.Maybe [Runtime],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PublishLayerVersionResponse' with the minimum fields required to make a request.
--
-- * 'layerVersionARN' - The ARN of the layer version.
-- * 'content' - Details about the layer version.
-- * 'createdDate' - The date that the layer version was created, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
-- * 'version' - The version number.
-- * 'licenseInfo' - The layer's software license.
-- * 'layerARN' - The ARN of the layer.
-- * 'description' - The description of the version.
-- * 'compatibleRuntimes' - The layer's compatible runtimes.
-- * 'responseStatus' - The response status code.
mkPublishLayerVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PublishLayerVersionResponse
mkPublishLayerVersionResponse pResponseStatus_ =
  PublishLayerVersionResponse'
    { layerVersionARN = Lude.Nothing,
      content = Lude.Nothing,
      createdDate = Lude.Nothing,
      version = Lude.Nothing,
      licenseInfo = Lude.Nothing,
      layerARN = Lude.Nothing,
      description = Lude.Nothing,
      compatibleRuntimes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the layer version.
--
-- /Note:/ Consider using 'layerVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvrsLayerVersionARN :: Lens.Lens' PublishLayerVersionResponse (Lude.Maybe Lude.Text)
plvrsLayerVersionARN = Lens.lens (layerVersionARN :: PublishLayerVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {layerVersionARN = a} :: PublishLayerVersionResponse)
{-# DEPRECATED plvrsLayerVersionARN "Use generic-lens or generic-optics with 'layerVersionARN' instead." #-}

-- | Details about the layer version.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvrsContent :: Lens.Lens' PublishLayerVersionResponse (Lude.Maybe LayerVersionContentOutput)
plvrsContent = Lens.lens (content :: PublishLayerVersionResponse -> Lude.Maybe LayerVersionContentOutput) (\s a -> s {content = a} :: PublishLayerVersionResponse)
{-# DEPRECATED plvrsContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The date that the layer version was created, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvrsCreatedDate :: Lens.Lens' PublishLayerVersionResponse (Lude.Maybe Lude.Text)
plvrsCreatedDate = Lens.lens (createdDate :: PublishLayerVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {createdDate = a} :: PublishLayerVersionResponse)
{-# DEPRECATED plvrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The version number.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvrsVersion :: Lens.Lens' PublishLayerVersionResponse (Lude.Maybe Lude.Integer)
plvrsVersion = Lens.lens (version :: PublishLayerVersionResponse -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: PublishLayerVersionResponse)
{-# DEPRECATED plvrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The layer's software license.
--
-- /Note:/ Consider using 'licenseInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvrsLicenseInfo :: Lens.Lens' PublishLayerVersionResponse (Lude.Maybe Lude.Text)
plvrsLicenseInfo = Lens.lens (licenseInfo :: PublishLayerVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {licenseInfo = a} :: PublishLayerVersionResponse)
{-# DEPRECATED plvrsLicenseInfo "Use generic-lens or generic-optics with 'licenseInfo' instead." #-}

-- | The ARN of the layer.
--
-- /Note:/ Consider using 'layerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvrsLayerARN :: Lens.Lens' PublishLayerVersionResponse (Lude.Maybe Lude.Text)
plvrsLayerARN = Lens.lens (layerARN :: PublishLayerVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {layerARN = a} :: PublishLayerVersionResponse)
{-# DEPRECATED plvrsLayerARN "Use generic-lens or generic-optics with 'layerARN' instead." #-}

-- | The description of the version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvrsDescription :: Lens.Lens' PublishLayerVersionResponse (Lude.Maybe Lude.Text)
plvrsDescription = Lens.lens (description :: PublishLayerVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PublishLayerVersionResponse)
{-# DEPRECATED plvrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The layer's compatible runtimes.
--
-- /Note:/ Consider using 'compatibleRuntimes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvrsCompatibleRuntimes :: Lens.Lens' PublishLayerVersionResponse (Lude.Maybe [Runtime])
plvrsCompatibleRuntimes = Lens.lens (compatibleRuntimes :: PublishLayerVersionResponse -> Lude.Maybe [Runtime]) (\s a -> s {compatibleRuntimes = a} :: PublishLayerVersionResponse)
{-# DEPRECATED plvrsCompatibleRuntimes "Use generic-lens or generic-optics with 'compatibleRuntimes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvrsResponseStatus :: Lens.Lens' PublishLayerVersionResponse Lude.Int
plvrsResponseStatus = Lens.lens (responseStatus :: PublishLayerVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PublishLayerVersionResponse)
{-# DEPRECATED plvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
