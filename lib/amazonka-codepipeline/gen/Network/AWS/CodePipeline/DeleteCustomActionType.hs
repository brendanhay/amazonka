{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.DeleteCustomActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Marks a custom action as deleted. @PollForJobs@ for the custom action fails after the action is marked for deletion. Used for custom actions only.
--
-- /Important:/ To re-create a custom action after it has been deleted you must use a string in the version field that has never been used before. This string can be an incremented version number, for example. To restore a deleted custom action, use a JSON file that is identical to the deleted action, including the original string in the version field.
module Network.AWS.CodePipeline.DeleteCustomActionType
  ( -- * Creating a request
    DeleteCustomActionType (..),
    mkDeleteCustomActionType,

    -- ** Request lenses
    dcatCategory,
    dcatProvider,
    dcatVersion,

    -- * Destructuring the response
    DeleteCustomActionTypeResponse (..),
    mkDeleteCustomActionTypeResponse,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DeleteCustomActionType@ operation. The custom action will be marked as deleted.
--
-- /See:/ 'mkDeleteCustomActionType' smart constructor.
data DeleteCustomActionType = DeleteCustomActionType'
  { category ::
      ActionCategory,
    provider :: Lude.Text,
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

-- | Creates a value of 'DeleteCustomActionType' with the minimum fields required to make a request.
--
-- * 'category' - The category of the custom action that you want to delete, such as source or deploy.
-- * 'provider' - The provider of the service used in the custom action, such as AWS CodeDeploy.
-- * 'version' - The version of the custom action to delete.
mkDeleteCustomActionType ::
  -- | 'category'
  ActionCategory ->
  -- | 'provider'
  Lude.Text ->
  -- | 'version'
  Lude.Text ->
  DeleteCustomActionType
mkDeleteCustomActionType pCategory_ pProvider_ pVersion_ =
  DeleteCustomActionType'
    { category = pCategory_,
      provider = pProvider_,
      version = pVersion_
    }

-- | The category of the custom action that you want to delete, such as source or deploy.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcatCategory :: Lens.Lens' DeleteCustomActionType ActionCategory
dcatCategory = Lens.lens (category :: DeleteCustomActionType -> ActionCategory) (\s a -> s {category = a} :: DeleteCustomActionType)
{-# DEPRECATED dcatCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | The provider of the service used in the custom action, such as AWS CodeDeploy.
--
-- /Note:/ Consider using 'provider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcatProvider :: Lens.Lens' DeleteCustomActionType Lude.Text
dcatProvider = Lens.lens (provider :: DeleteCustomActionType -> Lude.Text) (\s a -> s {provider = a} :: DeleteCustomActionType)
{-# DEPRECATED dcatProvider "Use generic-lens or generic-optics with 'provider' instead." #-}

-- | The version of the custom action to delete.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcatVersion :: Lens.Lens' DeleteCustomActionType Lude.Text
dcatVersion = Lens.lens (version :: DeleteCustomActionType -> Lude.Text) (\s a -> s {version = a} :: DeleteCustomActionType)
{-# DEPRECATED dcatVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.AWSRequest DeleteCustomActionType where
  type Rs DeleteCustomActionType = DeleteCustomActionTypeResponse
  request = Req.postJSON codePipelineService
  response = Res.receiveNull DeleteCustomActionTypeResponse'

instance Lude.ToHeaders DeleteCustomActionType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodePipeline_20150709.DeleteCustomActionType" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteCustomActionType where
  toJSON DeleteCustomActionType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("category" Lude..= category),
            Lude.Just ("provider" Lude..= provider),
            Lude.Just ("version" Lude..= version)
          ]
      )

instance Lude.ToPath DeleteCustomActionType where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCustomActionType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteCustomActionTypeResponse' smart constructor.
data DeleteCustomActionTypeResponse = DeleteCustomActionTypeResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCustomActionTypeResponse' with the minimum fields required to make a request.
mkDeleteCustomActionTypeResponse ::
  DeleteCustomActionTypeResponse
mkDeleteCustomActionTypeResponse = DeleteCustomActionTypeResponse'
