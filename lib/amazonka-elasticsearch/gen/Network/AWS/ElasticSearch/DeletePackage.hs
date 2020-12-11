{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DeletePackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the package.
module Network.AWS.ElasticSearch.DeletePackage
  ( -- * Creating a request
    DeletePackage (..),
    mkDeletePackage,

    -- ** Request lenses
    dPackageId,

    -- * Destructuring the response
    DeletePackageResponse (..),
    mkDeletePackageResponse,

    -- ** Response lenses
    delrsPackageDetails,
    delrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for request parameters to @'DeletePackage' @ operation.
--
-- /See:/ 'mkDeletePackage' smart constructor.
newtype DeletePackage = DeletePackage' {packageId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePackage' with the minimum fields required to make a request.
--
-- * 'packageId' - Internal ID of the package that you want to delete. Use @DescribePackages@ to find this value.
mkDeletePackage ::
  -- | 'packageId'
  Lude.Text ->
  DeletePackage
mkDeletePackage pPackageId_ =
  DeletePackage' {packageId = pPackageId_}

-- | Internal ID of the package that you want to delete. Use @DescribePackages@ to find this value.
--
-- /Note:/ Consider using 'packageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPackageId :: Lens.Lens' DeletePackage Lude.Text
dPackageId = Lens.lens (packageId :: DeletePackage -> Lude.Text) (\s a -> s {packageId = a} :: DeletePackage)
{-# DEPRECATED dPackageId "Use generic-lens or generic-optics with 'packageId' instead." #-}

instance Lude.AWSRequest DeletePackage where
  type Rs DeletePackage = DeletePackageResponse
  request = Req.delete elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeletePackageResponse'
            Lude.<$> (x Lude..?> "PackageDetails")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeletePackage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeletePackage where
  toPath DeletePackage' {..} =
    Lude.mconcat ["/2015-01-01/packages/", Lude.toBS packageId]

instance Lude.ToQuery DeletePackage where
  toQuery = Lude.const Lude.mempty

-- | Container for response parameters to @'DeletePackage' @ operation.
--
-- /See:/ 'mkDeletePackageResponse' smart constructor.
data DeletePackageResponse = DeletePackageResponse'
  { packageDetails ::
      Lude.Maybe PackageDetails,
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

-- | Creates a value of 'DeletePackageResponse' with the minimum fields required to make a request.
--
-- * 'packageDetails' - @PackageDetails@
-- * 'responseStatus' - The response status code.
mkDeletePackageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeletePackageResponse
mkDeletePackageResponse pResponseStatus_ =
  DeletePackageResponse'
    { packageDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | @PackageDetails@
--
-- /Note:/ Consider using 'packageDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsPackageDetails :: Lens.Lens' DeletePackageResponse (Lude.Maybe PackageDetails)
delrsPackageDetails = Lens.lens (packageDetails :: DeletePackageResponse -> Lude.Maybe PackageDetails) (\s a -> s {packageDetails = a} :: DeletePackageResponse)
{-# DEPRECATED delrsPackageDetails "Use generic-lens or generic-optics with 'packageDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsResponseStatus :: Lens.Lens' DeletePackageResponse Lude.Int
delrsResponseStatus = Lens.lens (responseStatus :: DeletePackageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeletePackageResponse)
{-# DEPRECATED delrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
