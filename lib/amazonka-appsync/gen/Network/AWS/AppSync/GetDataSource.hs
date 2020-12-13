{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.GetDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a @DataSource@ object.
module Network.AWS.AppSync.GetDataSource
  ( -- * Creating a request
    GetDataSource (..),
    mkGetDataSource,

    -- ** Request lenses
    gdsApiId,
    gdsName,

    -- * Destructuring the response
    GetDataSourceResponse (..),
    mkGetDataSourceResponse,

    -- ** Response lenses
    gdsrsDataSource,
    gdsrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDataSource' smart constructor.
data GetDataSource = GetDataSource'
  { -- | The API ID.
    apiId :: Lude.Text,
    -- | The name of the data source.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDataSource' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
-- * 'name' - The name of the data source.
mkGetDataSource ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  GetDataSource
mkGetDataSource pApiId_ pName_ =
  GetDataSource' {apiId = pApiId_, name = pName_}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsApiId :: Lens.Lens' GetDataSource Lude.Text
gdsApiId = Lens.lens (apiId :: GetDataSource -> Lude.Text) (\s a -> s {apiId = a} :: GetDataSource)
{-# DEPRECATED gdsApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The name of the data source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsName :: Lens.Lens' GetDataSource Lude.Text
gdsName = Lens.lens (name :: GetDataSource -> Lude.Text) (\s a -> s {name = a} :: GetDataSource)
{-# DEPRECATED gdsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetDataSource where
  type Rs GetDataSource = GetDataSourceResponse
  request = Req.get appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDataSourceResponse'
            Lude.<$> (x Lude..?> "dataSource") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDataSource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetDataSource where
  toPath GetDataSource' {..} =
    Lude.mconcat
      ["/v1/apis/", Lude.toBS apiId, "/datasources/", Lude.toBS name]

instance Lude.ToQuery GetDataSource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDataSourceResponse' smart constructor.
data GetDataSourceResponse = GetDataSourceResponse'
  { -- | The @DataSource@ object.
    dataSource :: Lude.Maybe DataSource,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDataSourceResponse' with the minimum fields required to make a request.
--
-- * 'dataSource' - The @DataSource@ object.
-- * 'responseStatus' - The response status code.
mkGetDataSourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDataSourceResponse
mkGetDataSourceResponse pResponseStatus_ =
  GetDataSourceResponse'
    { dataSource = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @DataSource@ object.
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsDataSource :: Lens.Lens' GetDataSourceResponse (Lude.Maybe DataSource)
gdsrsDataSource = Lens.lens (dataSource :: GetDataSourceResponse -> Lude.Maybe DataSource) (\s a -> s {dataSource = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsDataSource "Use generic-lens or generic-optics with 'dataSource' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsResponseStatus :: Lens.Lens' GetDataSourceResponse Lude.Int
gdsrsResponseStatus = Lens.lens (responseStatus :: GetDataSourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
