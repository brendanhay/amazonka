{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    ufDataSourceName,
    ufApiId,
    ufRequestMappingTemplate,
    ufName,
    ufFunctionId,
    ufResponseMappingTemplate,
    ufFunctionVersion,
    ufDescription,

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
  { -- | The @Function@ @DataSource@ name.
    dataSourceName :: Lude.Text,
    -- | The GraphQL API ID.
    apiId :: Lude.Text,
    -- | The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
    requestMappingTemplate :: Lude.Maybe Lude.Text,
    -- | The @Function@ name.
    name :: Lude.Text,
    -- | The function ID.
    functionId :: Lude.Text,
    -- | The @Function@ request mapping template.
    responseMappingTemplate :: Lude.Maybe Lude.Text,
    -- | The @version@ of the request mapping template. Currently the supported value is 2018-05-29.
    functionVersion :: Lude.Text,
    -- | The @Function@ description.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFunction' with the minimum fields required to make a request.
--
-- * 'dataSourceName' - The @Function@ @DataSource@ name.
-- * 'apiId' - The GraphQL API ID.
-- * 'requestMappingTemplate' - The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
-- * 'name' - The @Function@ name.
-- * 'functionId' - The function ID.
-- * 'responseMappingTemplate' - The @Function@ request mapping template.
-- * 'functionVersion' - The @version@ of the request mapping template. Currently the supported value is 2018-05-29.
-- * 'description' - The @Function@ description.
mkUpdateFunction ::
  -- | 'dataSourceName'
  Lude.Text ->
  -- | 'apiId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'functionId'
  Lude.Text ->
  -- | 'functionVersion'
  Lude.Text ->
  UpdateFunction
mkUpdateFunction
  pDataSourceName_
  pApiId_
  pName_
  pFunctionId_
  pFunctionVersion_ =
    UpdateFunction'
      { dataSourceName = pDataSourceName_,
        apiId = pApiId_,
        requestMappingTemplate = Lude.Nothing,
        name = pName_,
        functionId = pFunctionId_,
        responseMappingTemplate = Lude.Nothing,
        functionVersion = pFunctionVersion_,
        description = Lude.Nothing
      }

-- | The @Function@ @DataSource@ name.
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufDataSourceName :: Lens.Lens' UpdateFunction Lude.Text
ufDataSourceName = Lens.lens (dataSourceName :: UpdateFunction -> Lude.Text) (\s a -> s {dataSourceName = a} :: UpdateFunction)
{-# DEPRECATED ufDataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead." #-}

-- | The GraphQL API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufApiId :: Lens.Lens' UpdateFunction Lude.Text
ufApiId = Lens.lens (apiId :: UpdateFunction -> Lude.Text) (\s a -> s {apiId = a} :: UpdateFunction)
{-# DEPRECATED ufApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
--
-- /Note:/ Consider using 'requestMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufRequestMappingTemplate :: Lens.Lens' UpdateFunction (Lude.Maybe Lude.Text)
ufRequestMappingTemplate = Lens.lens (requestMappingTemplate :: UpdateFunction -> Lude.Maybe Lude.Text) (\s a -> s {requestMappingTemplate = a} :: UpdateFunction)
{-# DEPRECATED ufRequestMappingTemplate "Use generic-lens or generic-optics with 'requestMappingTemplate' instead." #-}

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

-- | The @Function@ request mapping template.
--
-- /Note:/ Consider using 'responseMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufResponseMappingTemplate :: Lens.Lens' UpdateFunction (Lude.Maybe Lude.Text)
ufResponseMappingTemplate = Lens.lens (responseMappingTemplate :: UpdateFunction -> Lude.Maybe Lude.Text) (\s a -> s {responseMappingTemplate = a} :: UpdateFunction)
{-# DEPRECATED ufResponseMappingTemplate "Use generic-lens or generic-optics with 'responseMappingTemplate' instead." #-}

-- | The @version@ of the request mapping template. Currently the supported value is 2018-05-29.
--
-- /Note:/ Consider using 'functionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufFunctionVersion :: Lens.Lens' UpdateFunction Lude.Text
ufFunctionVersion = Lens.lens (functionVersion :: UpdateFunction -> Lude.Text) (\s a -> s {functionVersion = a} :: UpdateFunction)
{-# DEPRECATED ufFunctionVersion "Use generic-lens or generic-optics with 'functionVersion' instead." #-}

-- | The @Function@ description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufDescription :: Lens.Lens' UpdateFunction (Lude.Maybe Lude.Text)
ufDescription = Lens.lens (description :: UpdateFunction -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateFunction)
{-# DEPRECATED ufDescription "Use generic-lens or generic-optics with 'description' instead." #-}

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
          [ Lude.Just ("dataSourceName" Lude..= dataSourceName),
            ("requestMappingTemplate" Lude..=) Lude.<$> requestMappingTemplate,
            Lude.Just ("name" Lude..= name),
            ("responseMappingTemplate" Lude..=)
              Lude.<$> responseMappingTemplate,
            Lude.Just ("functionVersion" Lude..= functionVersion),
            ("description" Lude..=) Lude.<$> description
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
  { -- | The @Function@ object.
    functionConfiguration :: Lude.Maybe FunctionConfiguration,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
