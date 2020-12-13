{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateUserDefinedFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new function definition in the Data Catalog.
module Network.AWS.Glue.CreateUserDefinedFunction
  ( -- * Creating a request
    CreateUserDefinedFunction (..),
    mkCreateUserDefinedFunction,

    -- ** Request lenses
    cudfCatalogId,
    cudfFunctionInput,
    cudfDatabaseName,

    -- * Destructuring the response
    CreateUserDefinedFunctionResponse (..),
    mkCreateUserDefinedFunctionResponse,

    -- ** Response lenses
    cudfrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateUserDefinedFunction' smart constructor.
data CreateUserDefinedFunction = CreateUserDefinedFunction'
  { -- | The ID of the Data Catalog in which to create the function. If none is provided, the AWS account ID is used by default.
    catalogId :: Lude.Maybe Lude.Text,
    -- | A @FunctionInput@ object that defines the function to create in the Data Catalog.
    functionInput :: UserDefinedFunctionInput,
    -- | The name of the catalog database in which to create the function.
    databaseName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserDefinedFunction' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog in which to create the function. If none is provided, the AWS account ID is used by default.
-- * 'functionInput' - A @FunctionInput@ object that defines the function to create in the Data Catalog.
-- * 'databaseName' - The name of the catalog database in which to create the function.
mkCreateUserDefinedFunction ::
  -- | 'functionInput'
  UserDefinedFunctionInput ->
  -- | 'databaseName'
  Lude.Text ->
  CreateUserDefinedFunction
mkCreateUserDefinedFunction pFunctionInput_ pDatabaseName_ =
  CreateUserDefinedFunction'
    { catalogId = Lude.Nothing,
      functionInput = pFunctionInput_,
      databaseName = pDatabaseName_
    }

-- | The ID of the Data Catalog in which to create the function. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cudfCatalogId :: Lens.Lens' CreateUserDefinedFunction (Lude.Maybe Lude.Text)
cudfCatalogId = Lens.lens (catalogId :: CreateUserDefinedFunction -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: CreateUserDefinedFunction)
{-# DEPRECATED cudfCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | A @FunctionInput@ object that defines the function to create in the Data Catalog.
--
-- /Note:/ Consider using 'functionInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cudfFunctionInput :: Lens.Lens' CreateUserDefinedFunction UserDefinedFunctionInput
cudfFunctionInput = Lens.lens (functionInput :: CreateUserDefinedFunction -> UserDefinedFunctionInput) (\s a -> s {functionInput = a} :: CreateUserDefinedFunction)
{-# DEPRECATED cudfFunctionInput "Use generic-lens or generic-optics with 'functionInput' instead." #-}

-- | The name of the catalog database in which to create the function.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cudfDatabaseName :: Lens.Lens' CreateUserDefinedFunction Lude.Text
cudfDatabaseName = Lens.lens (databaseName :: CreateUserDefinedFunction -> Lude.Text) (\s a -> s {databaseName = a} :: CreateUserDefinedFunction)
{-# DEPRECATED cudfDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

instance Lude.AWSRequest CreateUserDefinedFunction where
  type
    Rs CreateUserDefinedFunction =
      CreateUserDefinedFunctionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateUserDefinedFunctionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateUserDefinedFunction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.CreateUserDefinedFunction" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateUserDefinedFunction where
  toJSON CreateUserDefinedFunction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("FunctionInput" Lude..= functionInput),
            Lude.Just ("DatabaseName" Lude..= databaseName)
          ]
      )

instance Lude.ToPath CreateUserDefinedFunction where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateUserDefinedFunction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateUserDefinedFunctionResponse' smart constructor.
newtype CreateUserDefinedFunctionResponse = CreateUserDefinedFunctionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserDefinedFunctionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateUserDefinedFunctionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateUserDefinedFunctionResponse
mkCreateUserDefinedFunctionResponse pResponseStatus_ =
  CreateUserDefinedFunctionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cudfrsResponseStatus :: Lens.Lens' CreateUserDefinedFunctionResponse Lude.Int
cudfrsResponseStatus = Lens.lens (responseStatus :: CreateUserDefinedFunctionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateUserDefinedFunctionResponse)
{-# DEPRECATED cudfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
