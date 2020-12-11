{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateUserDefinedFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing function definition in the Data Catalog.
module Network.AWS.Glue.UpdateUserDefinedFunction
  ( -- * Creating a request
    UpdateUserDefinedFunction (..),
    mkUpdateUserDefinedFunction,

    -- ** Request lenses
    uudfCatalogId,
    uudfDatabaseName,
    uudfFunctionName,
    uudfFunctionInput,

    -- * Destructuring the response
    UpdateUserDefinedFunctionResponse (..),
    mkUpdateUserDefinedFunctionResponse,

    -- ** Response lenses
    uudfrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateUserDefinedFunction' smart constructor.
data UpdateUserDefinedFunction = UpdateUserDefinedFunction'
  { catalogId ::
      Lude.Maybe Lude.Text,
    databaseName :: Lude.Text,
    functionName :: Lude.Text,
    functionInput ::
      UserDefinedFunctionInput
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserDefinedFunction' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the function to be updated is located. If none is provided, the AWS account ID is used by default.
-- * 'databaseName' - The name of the catalog database where the function to be updated is located.
-- * 'functionInput' - A @FunctionInput@ object that redefines the function in the Data Catalog.
-- * 'functionName' - The name of the function.
mkUpdateUserDefinedFunction ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'functionName'
  Lude.Text ->
  -- | 'functionInput'
  UserDefinedFunctionInput ->
  UpdateUserDefinedFunction
mkUpdateUserDefinedFunction
  pDatabaseName_
  pFunctionName_
  pFunctionInput_ =
    UpdateUserDefinedFunction'
      { catalogId = Lude.Nothing,
        databaseName = pDatabaseName_,
        functionName = pFunctionName_,
        functionInput = pFunctionInput_
      }

-- | The ID of the Data Catalog where the function to be updated is located. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uudfCatalogId :: Lens.Lens' UpdateUserDefinedFunction (Lude.Maybe Lude.Text)
uudfCatalogId = Lens.lens (catalogId :: UpdateUserDefinedFunction -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: UpdateUserDefinedFunction)
{-# DEPRECATED uudfCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database where the function to be updated is located.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uudfDatabaseName :: Lens.Lens' UpdateUserDefinedFunction Lude.Text
uudfDatabaseName = Lens.lens (databaseName :: UpdateUserDefinedFunction -> Lude.Text) (\s a -> s {databaseName = a} :: UpdateUserDefinedFunction)
{-# DEPRECATED uudfDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the function.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uudfFunctionName :: Lens.Lens' UpdateUserDefinedFunction Lude.Text
uudfFunctionName = Lens.lens (functionName :: UpdateUserDefinedFunction -> Lude.Text) (\s a -> s {functionName = a} :: UpdateUserDefinedFunction)
{-# DEPRECATED uudfFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | A @FunctionInput@ object that redefines the function in the Data Catalog.
--
-- /Note:/ Consider using 'functionInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uudfFunctionInput :: Lens.Lens' UpdateUserDefinedFunction UserDefinedFunctionInput
uudfFunctionInput = Lens.lens (functionInput :: UpdateUserDefinedFunction -> UserDefinedFunctionInput) (\s a -> s {functionInput = a} :: UpdateUserDefinedFunction)
{-# DEPRECATED uudfFunctionInput "Use generic-lens or generic-optics with 'functionInput' instead." #-}

instance Lude.AWSRequest UpdateUserDefinedFunction where
  type
    Rs UpdateUserDefinedFunction =
      UpdateUserDefinedFunctionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateUserDefinedFunctionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateUserDefinedFunction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.UpdateUserDefinedFunction" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateUserDefinedFunction where
  toJSON UpdateUserDefinedFunction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("FunctionName" Lude..= functionName),
            Lude.Just ("FunctionInput" Lude..= functionInput)
          ]
      )

instance Lude.ToPath UpdateUserDefinedFunction where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateUserDefinedFunction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateUserDefinedFunctionResponse' smart constructor.
newtype UpdateUserDefinedFunctionResponse = UpdateUserDefinedFunctionResponse'
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

-- | Creates a value of 'UpdateUserDefinedFunctionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateUserDefinedFunctionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateUserDefinedFunctionResponse
mkUpdateUserDefinedFunctionResponse pResponseStatus_ =
  UpdateUserDefinedFunctionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uudfrsResponseStatus :: Lens.Lens' UpdateUserDefinedFunctionResponse Lude.Int
uudfrsResponseStatus = Lens.lens (responseStatus :: UpdateUserDefinedFunctionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateUserDefinedFunctionResponse)
{-# DEPRECATED uudfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
