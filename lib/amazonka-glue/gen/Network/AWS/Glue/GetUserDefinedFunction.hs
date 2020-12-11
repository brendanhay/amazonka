{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetUserDefinedFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specified function definition from the Data Catalog.
module Network.AWS.Glue.GetUserDefinedFunction
  ( -- * Creating a request
    GetUserDefinedFunction (..),
    mkGetUserDefinedFunction,

    -- ** Request lenses
    getCatalogId,
    getDatabaseName,
    getFunctionName,

    -- * Destructuring the response
    GetUserDefinedFunctionResponse (..),
    mkGetUserDefinedFunctionResponse,

    -- ** Response lenses
    gudfursUserDefinedFunction,
    gudfursResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetUserDefinedFunction' smart constructor.
data GetUserDefinedFunction = GetUserDefinedFunction'
  { catalogId ::
      Lude.Maybe Lude.Text,
    databaseName :: Lude.Text,
    functionName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUserDefinedFunction' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the function to be retrieved is located. If none is provided, the AWS account ID is used by default.
-- * 'databaseName' - The name of the catalog database where the function is located.
-- * 'functionName' - The name of the function.
mkGetUserDefinedFunction ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'functionName'
  Lude.Text ->
  GetUserDefinedFunction
mkGetUserDefinedFunction pDatabaseName_ pFunctionName_ =
  GetUserDefinedFunction'
    { catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      functionName = pFunctionName_
    }

-- | The ID of the Data Catalog where the function to be retrieved is located. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getCatalogId :: Lens.Lens' GetUserDefinedFunction (Lude.Maybe Lude.Text)
getCatalogId = Lens.lens (catalogId :: GetUserDefinedFunction -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetUserDefinedFunction)
{-# DEPRECATED getCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database where the function is located.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getDatabaseName :: Lens.Lens' GetUserDefinedFunction Lude.Text
getDatabaseName = Lens.lens (databaseName :: GetUserDefinedFunction -> Lude.Text) (\s a -> s {databaseName = a} :: GetUserDefinedFunction)
{-# DEPRECATED getDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the function.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getFunctionName :: Lens.Lens' GetUserDefinedFunction Lude.Text
getFunctionName = Lens.lens (functionName :: GetUserDefinedFunction -> Lude.Text) (\s a -> s {functionName = a} :: GetUserDefinedFunction)
{-# DEPRECATED getFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Lude.AWSRequest GetUserDefinedFunction where
  type Rs GetUserDefinedFunction = GetUserDefinedFunctionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetUserDefinedFunctionResponse'
            Lude.<$> (x Lude..?> "UserDefinedFunction")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetUserDefinedFunction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetUserDefinedFunction" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetUserDefinedFunction where
  toJSON GetUserDefinedFunction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("FunctionName" Lude..= functionName)
          ]
      )

instance Lude.ToPath GetUserDefinedFunction where
  toPath = Lude.const "/"

instance Lude.ToQuery GetUserDefinedFunction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetUserDefinedFunctionResponse' smart constructor.
data GetUserDefinedFunctionResponse = GetUserDefinedFunctionResponse'
  { userDefinedFunction ::
      Lude.Maybe
        UserDefinedFunction,
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

-- | Creates a value of 'GetUserDefinedFunctionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'userDefinedFunction' - The requested function definition.
mkGetUserDefinedFunctionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetUserDefinedFunctionResponse
mkGetUserDefinedFunctionResponse pResponseStatus_ =
  GetUserDefinedFunctionResponse'
    { userDefinedFunction =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The requested function definition.
--
-- /Note:/ Consider using 'userDefinedFunction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfursUserDefinedFunction :: Lens.Lens' GetUserDefinedFunctionResponse (Lude.Maybe UserDefinedFunction)
gudfursUserDefinedFunction = Lens.lens (userDefinedFunction :: GetUserDefinedFunctionResponse -> Lude.Maybe UserDefinedFunction) (\s a -> s {userDefinedFunction = a} :: GetUserDefinedFunctionResponse)
{-# DEPRECATED gudfursUserDefinedFunction "Use generic-lens or generic-optics with 'userDefinedFunction' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfursResponseStatus :: Lens.Lens' GetUserDefinedFunctionResponse Lude.Int
gudfursResponseStatus = Lens.lens (responseStatus :: GetUserDefinedFunctionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetUserDefinedFunctionResponse)
{-# DEPRECATED gudfursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
