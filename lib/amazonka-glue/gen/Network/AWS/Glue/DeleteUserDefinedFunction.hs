{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteUserDefinedFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing function definition from the Data Catalog.
module Network.AWS.Glue.DeleteUserDefinedFunction
  ( -- * Creating a request
    DeleteUserDefinedFunction (..),
    mkDeleteUserDefinedFunction,

    -- ** Request lenses
    dudfCatalogId,
    dudfDatabaseName,
    dudfFunctionName,

    -- * Destructuring the response
    DeleteUserDefinedFunctionResponse (..),
    mkDeleteUserDefinedFunctionResponse,

    -- ** Response lenses
    dudfrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteUserDefinedFunction' smart constructor.
data DeleteUserDefinedFunction = DeleteUserDefinedFunction'
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

-- | Creates a value of 'DeleteUserDefinedFunction' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the function to be deleted is located. If none is supplied, the AWS account ID is used by default.
-- * 'databaseName' - The name of the catalog database where the function is located.
-- * 'functionName' - The name of the function definition to be deleted.
mkDeleteUserDefinedFunction ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'functionName'
  Lude.Text ->
  DeleteUserDefinedFunction
mkDeleteUserDefinedFunction pDatabaseName_ pFunctionName_ =
  DeleteUserDefinedFunction'
    { catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      functionName = pFunctionName_
    }

-- | The ID of the Data Catalog where the function to be deleted is located. If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dudfCatalogId :: Lens.Lens' DeleteUserDefinedFunction (Lude.Maybe Lude.Text)
dudfCatalogId = Lens.lens (catalogId :: DeleteUserDefinedFunction -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: DeleteUserDefinedFunction)
{-# DEPRECATED dudfCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database where the function is located.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dudfDatabaseName :: Lens.Lens' DeleteUserDefinedFunction Lude.Text
dudfDatabaseName = Lens.lens (databaseName :: DeleteUserDefinedFunction -> Lude.Text) (\s a -> s {databaseName = a} :: DeleteUserDefinedFunction)
{-# DEPRECATED dudfDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the function definition to be deleted.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dudfFunctionName :: Lens.Lens' DeleteUserDefinedFunction Lude.Text
dudfFunctionName = Lens.lens (functionName :: DeleteUserDefinedFunction -> Lude.Text) (\s a -> s {functionName = a} :: DeleteUserDefinedFunction)
{-# DEPRECATED dudfFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Lude.AWSRequest DeleteUserDefinedFunction where
  type
    Rs DeleteUserDefinedFunction =
      DeleteUserDefinedFunctionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteUserDefinedFunctionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteUserDefinedFunction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.DeleteUserDefinedFunction" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteUserDefinedFunction where
  toJSON DeleteUserDefinedFunction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("FunctionName" Lude..= functionName)
          ]
      )

instance Lude.ToPath DeleteUserDefinedFunction where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteUserDefinedFunction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteUserDefinedFunctionResponse' smart constructor.
newtype DeleteUserDefinedFunctionResponse = DeleteUserDefinedFunctionResponse'
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

-- | Creates a value of 'DeleteUserDefinedFunctionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteUserDefinedFunctionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteUserDefinedFunctionResponse
mkDeleteUserDefinedFunctionResponse pResponseStatus_ =
  DeleteUserDefinedFunctionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dudfrsResponseStatus :: Lens.Lens' DeleteUserDefinedFunctionResponse Lude.Int
dudfrsResponseStatus = Lens.lens (responseStatus :: DeleteUserDefinedFunctionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteUserDefinedFunctionResponse)
{-# DEPRECATED dudfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
