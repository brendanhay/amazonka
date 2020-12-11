{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteModelPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a model package.
--
-- A model package is used to create Amazon SageMaker models or list on AWS Marketplace. Buyers can subscribe to model packages listed on AWS Marketplace to create models in Amazon SageMaker.
module Network.AWS.SageMaker.DeleteModelPackage
  ( -- * Creating a request
    DeleteModelPackage (..),
    mkDeleteModelPackage,

    -- ** Request lenses
    dmpModelPackageName,

    -- * Destructuring the response
    DeleteModelPackageResponse (..),
    mkDeleteModelPackageResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteModelPackage' smart constructor.
newtype DeleteModelPackage = DeleteModelPackage'
  { modelPackageName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteModelPackage' with the minimum fields required to make a request.
--
-- * 'modelPackageName' - The name of the model package. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
mkDeleteModelPackage ::
  -- | 'modelPackageName'
  Lude.Text ->
  DeleteModelPackage
mkDeleteModelPackage pModelPackageName_ =
  DeleteModelPackage' {modelPackageName = pModelPackageName_}

-- | The name of the model package. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
--
-- /Note:/ Consider using 'modelPackageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpModelPackageName :: Lens.Lens' DeleteModelPackage Lude.Text
dmpModelPackageName = Lens.lens (modelPackageName :: DeleteModelPackage -> Lude.Text) (\s a -> s {modelPackageName = a} :: DeleteModelPackage)
{-# DEPRECATED dmpModelPackageName "Use generic-lens or generic-optics with 'modelPackageName' instead." #-}

instance Lude.AWSRequest DeleteModelPackage where
  type Rs DeleteModelPackage = DeleteModelPackageResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull DeleteModelPackageResponse'

instance Lude.ToHeaders DeleteModelPackage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteModelPackage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteModelPackage where
  toJSON DeleteModelPackage' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ModelPackageName" Lude..= modelPackageName)]
      )

instance Lude.ToPath DeleteModelPackage where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteModelPackage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteModelPackageResponse' smart constructor.
data DeleteModelPackageResponse = DeleteModelPackageResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteModelPackageResponse' with the minimum fields required to make a request.
mkDeleteModelPackageResponse ::
  DeleteModelPackageResponse
mkDeleteModelPackageResponse = DeleteModelPackageResponse'
