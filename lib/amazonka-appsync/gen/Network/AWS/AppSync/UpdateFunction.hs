{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.UpdateFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a @Function@ object.
module Network.AWS.AppSync.UpdateFunction
  ( -- * Creating a request
    UpdateFunction (..),
    mkUpdateFunction,

    -- ** Request lenses
    ufRequestMappingTemplate,
    ufResponseMappingTemplate,
    ufDescription,
    ufApiId,
    ufName,
    ufFunctionId,
    ufDataSourceName,
    ufFunctionVersion,

    -- * Destructuring the response
    UpdateFunctionResponse (..),
    mkUpdateFunctionResponse,

    -- ** Response lenses
    ufrsFunctionConfiguration,
    ufrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateFunction' smart constructor.
data UpdateFunction = UpdateFunction'
  { requestMappingTemplate ::
      Lude.Maybe Lude.Text,
    responseMappingTemplate :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    apiId :: Lude.Text,
    name :: Lude.Text,
    functionId :: Lude.Text,
    dataSourceName :: Lude.Text,
    functionVersion :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFunction' with the minimum fields required to make a request.
--
-- * 'apiId' - The GraphQL API ID.
-- * 'dataSourceName' - The @Function@ @DataSource@ name.
-- * 'description' - The @Function@ description.
-- * 'functionId' - The function ID.
-- * 'functionVersion' - The @version@ of the request mapping template. Currently the supported value is 2018-05-29.
-- * 'name' - The @Function@ name.
-- * 'requestMappingTemplate' - The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
-- * 'responseMappingTemplate' - The @Function@ request mapping template.
mkUpdateFunction ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'functionId'
  Lude.Text ->
  -- | 'dataSourceName'
  Lude.Text ->
  -- | 'functionVersion'
  Lude.Text ->
  UpdateFunction
mkUpdateFunction
  pApiId_
  pName_
  pFunctionId_
  pDataSourceName_
  pFunctionVersion_ =
    UpdateFunction'
      { requestMappingTemplate = Lude.Nothing,
        responseMappingTemplate = Lude.Nothing,
        description = Lude.Nothing,
        apiId = pApiId_,
        name = pName_,
        functionId = pFunctionId_,
        dataSourceName = pDataSourceName_,
        functionVersion = pFunctionVersion_
      }

-- | The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
--
-- /Note:/ Consider using 'requestMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufRequestMappingTemplate :: Lens.Lens' UpdateFunction (Lude.Maybe Lude.Text)
ufRequestMappingTemplate = Lens.lens (requestMappingTemplate :: UpdateFunction -> Lude.Maybe Lude.Text) (\s a -> s {requestMappingTemplate = a} :: UpdateFunction)
{-# DEPRECATED ufRequestMappingTemplate "Use generic-lens or generic-optics with 'requestMappingTemplate' instead." #-}

-- | The @Function@ request mapping template.
--
-- /Note:/ Consider using 'responseMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufResponseMappingTemplate :: Lens.Lens' UpdateFunction (Lude.Maybe Lude.Text)
ufResponseMappingTemplate = Lens.lens (responseMappingTemplate :: UpdateFunction -> Lude.Maybe Lude.Text) (\s a -> s {responseMappingTemplate = a} :: UpdateFunction)
{-# DEPRECATED ufResponseMappingTemplate "Use generic-lens or generic-optics with 'responseMappingTemplate' instead." #-}

-- | The @Function@ description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufDescription :: Lens.Lens' UpdateFunction (Lude.Maybe Lude.Text)
ufDescription = Lens.lens (description :: UpdateFunction -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateFunction)
{-# DEPRECATED ufDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The GraphQL API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufApiId :: Lens.Lens' UpdateFunction Lude.Text
ufApiId = Lens.lens (apiId :: UpdateFunction -> Lude.Text) (\s a -> s {apiId = a} :: UpdateFunction)
{-# DEPRECATED ufApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The @Function@ name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufName :: Lens.Lens' UpdateFunction Lude.Text
ufName = Lens.lens (name :: UpdateFunction -> Lude.Text) (\s a -> s {name = a} :: UpdateFunction)
{-# DEPRECATED ufName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The function ID.
--
-- /Note:/ Consider using 'functionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufFunctionId :: Lens.Lens' UpdateFunction Lude.Text
ufFunctionId = Lens.lens (functionId :: UpdateFunction -> Lude.Text) (\s a -> s {functionId = a} :: UpdateFunction)
{-# DEPRECATED ufFunctionId "Use generic-lens or generic-optics with 'functionId' instead." #-}

-- | The @Function@ @DataSource@ name.
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufDataSourceName :: Lens.Lens' UpdateFunction Lude.Text
ufDataSourceName = Lens.lens (dataSourceName :: UpdateFunction -> Lude.Text) (\s a -> s {dataSourceName = a} :: UpdateFunction)
{-# DEPRECATED ufDataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead." #-}

-- | The @version@ of the request mapping template. Currently the supported value is 2018-05-29.
--
-- /Note:/ Consider using 'functionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufFunctionVersion :: Lens.Lens' UpdateFunction Lude.Text
ufFunctionVersion = Lens.lens (functionVersion :: UpdateFunction -> Lude.Text) (\s a -> s {functionVersion = a} :: UpdateFunction)
{-# DEPRECATED ufFunctionVersion "Use generic-lens or generic-optics with 'functionVersion' instead." #-}

instance Lude.AWSRequest UpdateFunction where
  type Rs UpdateFunction = UpdateFunctionResponse
  request = Req.postJSON appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateFunctionResponse'
            Lude.<$> (x Lude..?> "functionConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateFunction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateFunction where
  toJSON UpdateFunction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("requestMappingTemplate" Lude..=)
              Lude.<$> requestMappingTemplate,
            ("responseMappingTemplate" Lude..=)
              Lude.<$> responseMappingTemplate,
            ("description" Lude..=) Lude.<$> description,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("dataSourceName" Lude..= dataSourceName),
            Lude.Just ("functionVersion" Lude..= functionVersion)
          ]
      )

instance Lude.ToPath UpdateFunction where
  toPath UpdateFunction' {..} =
    Lude.mconcat
      ["/v1/apis/", Lude.toBS apiId, "/functions/", Lude.toBS functionId]

instance Lude.ToQuery UpdateFunction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateFunctionResponse' smart constructor.
data UpdateFunctionResponse = UpdateFunctionResponse'
  { functionConfiguration ::
      Lude.Maybe FunctionConfiguration,
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

-- | Creates a value of 'UpdateFunctionResponse' with the minimum fields required to make a request.
--
-- * 'functionConfiguration' - The @Function@ object.
-- * 'responseStatus' - The response status code.
mkUpdateFunctionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateFunctionResponse
mkUpdateFunctionResponse pResponseStatus_ =
  UpdateFunctionResponse'
    { functionConfiguration = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @Function@ object.
--
-- /Note:/ Consider using 'functionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufrsFunctionConfiguration :: Lens.Lens' UpdateFunctionResponse (Lude.Maybe FunctionConfiguration)
ufrsFunctionConfiguration = Lens.lens (functionConfiguration :: UpdateFunctionResponse -> Lude.Maybe FunctionConfiguration) (\s a -> s {functionConfiguration = a} :: UpdateFunctionResponse)
{-# DEPRECATED ufrsFunctionConfiguration "Use generic-lens or generic-optics with 'functionConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufrsResponseStatus :: Lens.Lens' UpdateFunctionResponse Lude.Int
ufrsResponseStatus = Lens.lens (responseStatus :: UpdateFunctionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateFunctionResponse)
{-# DEPRECATED ufrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
