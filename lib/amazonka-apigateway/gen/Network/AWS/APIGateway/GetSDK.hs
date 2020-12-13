{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetSDK
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a client SDK for a 'RestApi' and 'Stage' .
module Network.AWS.APIGateway.GetSDK
  ( -- * Creating a request
    GetSDK (..),
    mkGetSDK,

    -- ** Request lenses
    gsdkSdkType,
    gsdkParameters,
    gsdkRestAPIId,
    gsdkStageName,

    -- * Destructuring the response
    GetSDKResponse (..),
    mkGetSDKResponse,

    -- ** Response lenses
    gsrsBody,
    gsrsContentDisposition,
    gsrsContentType,
    gsrsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request a new generated client SDK for a 'RestApi' and 'Stage' .
--
-- /See:/ 'mkGetSDK' smart constructor.
data GetSDK = GetSDK'
  { -- | [Required] The language for the generated SDK. Currently @java@ , @javascript@ , @android@ , @objectivec@ (for iOS), @swift@ (for iOS), and @ruby@ are supported.
    sdkType :: Lude.Text,
    -- | A string-to-string key-value map of query parameters @sdkType@ -dependent properties of the SDK. For @sdkType@ of @objectivec@ or @swift@ , a parameter named @classPrefix@ is required. For @sdkType@ of @android@ , parameters named @groupId@ , @artifactId@ , @artifactVersion@ , and @invokerPackage@ are required. For @sdkType@ of @java@ , parameters named @serviceName@ and @javaPackageName@ are required.
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | [Required] The name of the 'Stage' that the SDK will use.
    stageName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSDK' with the minimum fields required to make a request.
--
-- * 'sdkType' - [Required] The language for the generated SDK. Currently @java@ , @javascript@ , @android@ , @objectivec@ (for iOS), @swift@ (for iOS), and @ruby@ are supported.
-- * 'parameters' - A string-to-string key-value map of query parameters @sdkType@ -dependent properties of the SDK. For @sdkType@ of @objectivec@ or @swift@ , a parameter named @classPrefix@ is required. For @sdkType@ of @android@ , parameters named @groupId@ , @artifactId@ , @artifactVersion@ , and @invokerPackage@ are required. For @sdkType@ of @java@ , parameters named @serviceName@ and @javaPackageName@ are required.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'stageName' - [Required] The name of the 'Stage' that the SDK will use.
mkGetSDK ::
  -- | 'sdkType'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'stageName'
  Lude.Text ->
  GetSDK
mkGetSDK pSdkType_ pRestAPIId_ pStageName_ =
  GetSDK'
    { sdkType = pSdkType_,
      parameters = Lude.Nothing,
      restAPIId = pRestAPIId_,
      stageName = pStageName_
    }

-- | [Required] The language for the generated SDK. Currently @java@ , @javascript@ , @android@ , @objectivec@ (for iOS), @swift@ (for iOS), and @ruby@ are supported.
--
-- /Note:/ Consider using 'sdkType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdkSdkType :: Lens.Lens' GetSDK Lude.Text
gsdkSdkType = Lens.lens (sdkType :: GetSDK -> Lude.Text) (\s a -> s {sdkType = a} :: GetSDK)
{-# DEPRECATED gsdkSdkType "Use generic-lens or generic-optics with 'sdkType' instead." #-}

-- | A string-to-string key-value map of query parameters @sdkType@ -dependent properties of the SDK. For @sdkType@ of @objectivec@ or @swift@ , a parameter named @classPrefix@ is required. For @sdkType@ of @android@ , parameters named @groupId@ , @artifactId@ , @artifactVersion@ , and @invokerPackage@ are required. For @sdkType@ of @java@ , parameters named @serviceName@ and @javaPackageName@ are required.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdkParameters :: Lens.Lens' GetSDK (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gsdkParameters = Lens.lens (parameters :: GetSDK -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: GetSDK)
{-# DEPRECATED gsdkParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdkRestAPIId :: Lens.Lens' GetSDK Lude.Text
gsdkRestAPIId = Lens.lens (restAPIId :: GetSDK -> Lude.Text) (\s a -> s {restAPIId = a} :: GetSDK)
{-# DEPRECATED gsdkRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The name of the 'Stage' that the SDK will use.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdkStageName :: Lens.Lens' GetSDK Lude.Text
gsdkStageName = Lens.lens (stageName :: GetSDK -> Lude.Text) (\s a -> s {stageName = a} :: GetSDK)
{-# DEPRECATED gsdkStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

instance Lude.AWSRequest GetSDK where
  type Rs GetSDK = GetSDKResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveBytes
      ( \s h x ->
          GetSDKResponse'
            Lude.<$> (Lude.pure (Lude.Just x))
            Lude.<*> (h Lude..#? "Content-Disposition")
            Lude.<*> (h Lude..#? "Content-Type")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSDK where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetSDK where
  toPath GetSDK' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/stages/",
        Lude.toBS stageName,
        "/sdks/",
        Lude.toBS sdkType
      ]

instance Lude.ToQuery GetSDK where
  toQuery GetSDK' {..} =
    Lude.mconcat
      [ "parameters"
          Lude.=: Lude.toQuery
            (Lude.toQueryMap "entry" "key" "value" Lude.<$> parameters)
      ]

-- | The binary blob response to 'GetSdk' , which contains the generated SDK.
--
-- /See:/ 'mkGetSDKResponse' smart constructor.
data GetSDKResponse = GetSDKResponse'
  { -- | The binary blob response to 'GetSdk' , which contains the generated SDK.
    body :: Lude.Maybe Lude.ByteString,
    -- | The content-disposition header value in the HTTP response.
    contentDisposition :: Lude.Maybe Lude.Text,
    -- | The content-type header value in the HTTP response.
    contentType :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSDKResponse' with the minimum fields required to make a request.
--
-- * 'body' - The binary blob response to 'GetSdk' , which contains the generated SDK.
-- * 'contentDisposition' - The content-disposition header value in the HTTP response.
-- * 'contentType' - The content-type header value in the HTTP response.
-- * 'responseStatus' - The response status code.
mkGetSDKResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSDKResponse
mkGetSDKResponse pResponseStatus_ =
  GetSDKResponse'
    { body = Lude.Nothing,
      contentDisposition = Lude.Nothing,
      contentType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The binary blob response to 'GetSdk' , which contains the generated SDK.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsBody :: Lens.Lens' GetSDKResponse (Lude.Maybe Lude.ByteString)
gsrsBody = Lens.lens (body :: GetSDKResponse -> Lude.Maybe Lude.ByteString) (\s a -> s {body = a} :: GetSDKResponse)
{-# DEPRECATED gsrsBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The content-disposition header value in the HTTP response.
--
-- /Note:/ Consider using 'contentDisposition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsContentDisposition :: Lens.Lens' GetSDKResponse (Lude.Maybe Lude.Text)
gsrsContentDisposition = Lens.lens (contentDisposition :: GetSDKResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentDisposition = a} :: GetSDKResponse)
{-# DEPRECATED gsrsContentDisposition "Use generic-lens or generic-optics with 'contentDisposition' instead." #-}

-- | The content-type header value in the HTTP response.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsContentType :: Lens.Lens' GetSDKResponse (Lude.Maybe Lude.Text)
gsrsContentType = Lens.lens (contentType :: GetSDKResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: GetSDKResponse)
{-# DEPRECATED gsrsContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsResponseStatus :: Lens.Lens' GetSDKResponse Lude.Int
gsrsResponseStatus = Lens.lens (responseStatus :: GetSDKResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSDKResponse)
{-# DEPRECATED gsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
