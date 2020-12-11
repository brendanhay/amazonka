{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.UpdatePackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a package for use with Amazon ES domains.
module Network.AWS.ElasticSearch.UpdatePackage
  ( -- * Creating a request
    UpdatePackage (..),
    mkUpdatePackage,

    -- ** Request lenses
    upPackageDescription,
    upCommitMessage,
    upPackageId,
    upPackageSource,

    -- * Destructuring the response
    UpdatePackageResponse (..),
    mkUpdatePackageResponse,

    -- ** Response lenses
    uprsPackageDetails,
    uprsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for request parameters to @'UpdatePackage' @ operation.
--
-- /See:/ 'mkUpdatePackage' smart constructor.
data UpdatePackage = UpdatePackage'
  { packageDescription ::
      Lude.Maybe Lude.Text,
    commitMessage :: Lude.Maybe Lude.Text,
    packageId :: Lude.Text,
    packageSource :: PackageSource
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePackage' with the minimum fields required to make a request.
--
-- * 'commitMessage' - An info message for the new version which will be shown as part of @GetPackageVersionHistoryResponse@ .
-- * 'packageDescription' - New description of the package.
-- * 'packageId' - Unique identifier for the package.
-- * 'packageSource' - Undocumented field.
mkUpdatePackage ::
  -- | 'packageId'
  Lude.Text ->
  -- | 'packageSource'
  PackageSource ->
  UpdatePackage
mkUpdatePackage pPackageId_ pPackageSource_ =
  UpdatePackage'
    { packageDescription = Lude.Nothing,
      commitMessage = Lude.Nothing,
      packageId = pPackageId_,
      packageSource = pPackageSource_
    }

-- | New description of the package.
--
-- /Note:/ Consider using 'packageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPackageDescription :: Lens.Lens' UpdatePackage (Lude.Maybe Lude.Text)
upPackageDescription = Lens.lens (packageDescription :: UpdatePackage -> Lude.Maybe Lude.Text) (\s a -> s {packageDescription = a} :: UpdatePackage)
{-# DEPRECATED upPackageDescription "Use generic-lens or generic-optics with 'packageDescription' instead." #-}

-- | An info message for the new version which will be shown as part of @GetPackageVersionHistoryResponse@ .
--
-- /Note:/ Consider using 'commitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upCommitMessage :: Lens.Lens' UpdatePackage (Lude.Maybe Lude.Text)
upCommitMessage = Lens.lens (commitMessage :: UpdatePackage -> Lude.Maybe Lude.Text) (\s a -> s {commitMessage = a} :: UpdatePackage)
{-# DEPRECATED upCommitMessage "Use generic-lens or generic-optics with 'commitMessage' instead." #-}

-- | Unique identifier for the package.
--
-- /Note:/ Consider using 'packageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPackageId :: Lens.Lens' UpdatePackage Lude.Text
upPackageId = Lens.lens (packageId :: UpdatePackage -> Lude.Text) (\s a -> s {packageId = a} :: UpdatePackage)
{-# DEPRECATED upPackageId "Use generic-lens or generic-optics with 'packageId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'packageSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPackageSource :: Lens.Lens' UpdatePackage PackageSource
upPackageSource = Lens.lens (packageSource :: UpdatePackage -> PackageSource) (\s a -> s {packageSource = a} :: UpdatePackage)
{-# DEPRECATED upPackageSource "Use generic-lens or generic-optics with 'packageSource' instead." #-}

instance Lude.AWSRequest UpdatePackage where
  type Rs UpdatePackage = UpdatePackageResponse
  request = Req.postJSON elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdatePackageResponse'
            Lude.<$> (x Lude..?> "PackageDetails")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdatePackage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdatePackage where
  toJSON UpdatePackage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PackageDescription" Lude..=) Lude.<$> packageDescription,
            ("CommitMessage" Lude..=) Lude.<$> commitMessage,
            Lude.Just ("PackageID" Lude..= packageId),
            Lude.Just ("PackageSource" Lude..= packageSource)
          ]
      )

instance Lude.ToPath UpdatePackage where
  toPath = Lude.const "/2015-01-01/packages/update"

instance Lude.ToQuery UpdatePackage where
  toQuery = Lude.const Lude.mempty

-- | Container for response returned by @'UpdatePackage' @ operation.
--
-- /See:/ 'mkUpdatePackageResponse' smart constructor.
data UpdatePackageResponse = UpdatePackageResponse'
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

-- | Creates a value of 'UpdatePackageResponse' with the minimum fields required to make a request.
--
-- * 'packageDetails' - Information about the package @PackageDetails@ .
-- * 'responseStatus' - The response status code.
mkUpdatePackageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdatePackageResponse
mkUpdatePackageResponse pResponseStatus_ =
  UpdatePackageResponse'
    { packageDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the package @PackageDetails@ .
--
-- /Note:/ Consider using 'packageDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsPackageDetails :: Lens.Lens' UpdatePackageResponse (Lude.Maybe PackageDetails)
uprsPackageDetails = Lens.lens (packageDetails :: UpdatePackageResponse -> Lude.Maybe PackageDetails) (\s a -> s {packageDetails = a} :: UpdatePackageResponse)
{-# DEPRECATED uprsPackageDetails "Use generic-lens or generic-optics with 'packageDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsResponseStatus :: Lens.Lens' UpdatePackageResponse Lude.Int
uprsResponseStatus = Lens.lens (responseStatus :: UpdatePackageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdatePackageResponse)
{-# DEPRECATED uprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
