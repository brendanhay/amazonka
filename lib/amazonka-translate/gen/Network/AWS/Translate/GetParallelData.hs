{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.GetParallelData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about a parallel data resource.
module Network.AWS.Translate.GetParallelData
  ( -- * Creating a request
    GetParallelData (..),
    mkGetParallelData,

    -- ** Request lenses
    gpdName,

    -- * Destructuring the response
    GetParallelDataResponse (..),
    mkGetParallelDataResponse,

    -- ** Response lenses
    gpdrsParallelDataProperties,
    gpdrsDataLocation,
    gpdrsAuxiliaryDataLocation,
    gpdrsLatestUpdateAttemptAuxiliaryDataLocation,
    gpdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Translate.Types

-- | /See:/ 'mkGetParallelData' smart constructor.
newtype GetParallelData = GetParallelData'
  { -- | The name of the parallel data resource that is being retrieved.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetParallelData' with the minimum fields required to make a request.
--
-- * 'name' - The name of the parallel data resource that is being retrieved.
mkGetParallelData ::
  -- | 'name'
  Lude.Text ->
  GetParallelData
mkGetParallelData pName_ = GetParallelData' {name = pName_}

-- | The name of the parallel data resource that is being retrieved.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdName :: Lens.Lens' GetParallelData Lude.Text
gpdName = Lens.lens (name :: GetParallelData -> Lude.Text) (\s a -> s {name = a} :: GetParallelData)
{-# DEPRECATED gpdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetParallelData where
  type Rs GetParallelData = GetParallelDataResponse
  request = Req.postJSON translateService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetParallelDataResponse'
            Lude.<$> (x Lude..?> "ParallelDataProperties")
            Lude.<*> (x Lude..?> "DataLocation")
            Lude.<*> (x Lude..?> "AuxiliaryDataLocation")
            Lude.<*> (x Lude..?> "LatestUpdateAttemptAuxiliaryDataLocation")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetParallelData where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShineFrontendService_20170701.GetParallelData" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetParallelData where
  toJSON GetParallelData' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath GetParallelData where
  toPath = Lude.const "/"

instance Lude.ToQuery GetParallelData where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetParallelDataResponse' smart constructor.
data GetParallelDataResponse = GetParallelDataResponse'
  { -- | The properties of the parallel data resource that is being retrieved.
    parallelDataProperties :: Lude.Maybe ParallelDataProperties,
    -- | The location of the most recent parallel data input file that was successfully imported into Amazon Translate. The location is returned as a presigned URL that has a 30 minute expiration.
    dataLocation :: Lude.Maybe ParallelDataDataLocation,
    -- | The Amazon S3 location of a file that provides any errors or warnings that were produced by your input file. This file was created when Amazon Translate attempted to create a parallel data resource. The location is returned as a presigned URL to that has a 30 minute expiration.
    auxiliaryDataLocation :: Lude.Maybe ParallelDataDataLocation,
    -- | The Amazon S3 location of a file that provides any errors or warnings that were produced by your input file. This file was created when Amazon Translate attempted to update a parallel data resource. The location is returned as a presigned URL to that has a 30 minute expiration.
    latestUpdateAttemptAuxiliaryDataLocation :: Lude.Maybe ParallelDataDataLocation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetParallelDataResponse' with the minimum fields required to make a request.
--
-- * 'parallelDataProperties' - The properties of the parallel data resource that is being retrieved.
-- * 'dataLocation' - The location of the most recent parallel data input file that was successfully imported into Amazon Translate. The location is returned as a presigned URL that has a 30 minute expiration.
-- * 'auxiliaryDataLocation' - The Amazon S3 location of a file that provides any errors or warnings that were produced by your input file. This file was created when Amazon Translate attempted to create a parallel data resource. The location is returned as a presigned URL to that has a 30 minute expiration.
-- * 'latestUpdateAttemptAuxiliaryDataLocation' - The Amazon S3 location of a file that provides any errors or warnings that were produced by your input file. This file was created when Amazon Translate attempted to update a parallel data resource. The location is returned as a presigned URL to that has a 30 minute expiration.
-- * 'responseStatus' - The response status code.
mkGetParallelDataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetParallelDataResponse
mkGetParallelDataResponse pResponseStatus_ =
  GetParallelDataResponse'
    { parallelDataProperties = Lude.Nothing,
      dataLocation = Lude.Nothing,
      auxiliaryDataLocation = Lude.Nothing,
      latestUpdateAttemptAuxiliaryDataLocation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The properties of the parallel data resource that is being retrieved.
--
-- /Note:/ Consider using 'parallelDataProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrsParallelDataProperties :: Lens.Lens' GetParallelDataResponse (Lude.Maybe ParallelDataProperties)
gpdrsParallelDataProperties = Lens.lens (parallelDataProperties :: GetParallelDataResponse -> Lude.Maybe ParallelDataProperties) (\s a -> s {parallelDataProperties = a} :: GetParallelDataResponse)
{-# DEPRECATED gpdrsParallelDataProperties "Use generic-lens or generic-optics with 'parallelDataProperties' instead." #-}

-- | The location of the most recent parallel data input file that was successfully imported into Amazon Translate. The location is returned as a presigned URL that has a 30 minute expiration.
--
-- /Note:/ Consider using 'dataLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrsDataLocation :: Lens.Lens' GetParallelDataResponse (Lude.Maybe ParallelDataDataLocation)
gpdrsDataLocation = Lens.lens (dataLocation :: GetParallelDataResponse -> Lude.Maybe ParallelDataDataLocation) (\s a -> s {dataLocation = a} :: GetParallelDataResponse)
{-# DEPRECATED gpdrsDataLocation "Use generic-lens or generic-optics with 'dataLocation' instead." #-}

-- | The Amazon S3 location of a file that provides any errors or warnings that were produced by your input file. This file was created when Amazon Translate attempted to create a parallel data resource. The location is returned as a presigned URL to that has a 30 minute expiration.
--
-- /Note:/ Consider using 'auxiliaryDataLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrsAuxiliaryDataLocation :: Lens.Lens' GetParallelDataResponse (Lude.Maybe ParallelDataDataLocation)
gpdrsAuxiliaryDataLocation = Lens.lens (auxiliaryDataLocation :: GetParallelDataResponse -> Lude.Maybe ParallelDataDataLocation) (\s a -> s {auxiliaryDataLocation = a} :: GetParallelDataResponse)
{-# DEPRECATED gpdrsAuxiliaryDataLocation "Use generic-lens or generic-optics with 'auxiliaryDataLocation' instead." #-}

-- | The Amazon S3 location of a file that provides any errors or warnings that were produced by your input file. This file was created when Amazon Translate attempted to update a parallel data resource. The location is returned as a presigned URL to that has a 30 minute expiration.
--
-- /Note:/ Consider using 'latestUpdateAttemptAuxiliaryDataLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrsLatestUpdateAttemptAuxiliaryDataLocation :: Lens.Lens' GetParallelDataResponse (Lude.Maybe ParallelDataDataLocation)
gpdrsLatestUpdateAttemptAuxiliaryDataLocation = Lens.lens (latestUpdateAttemptAuxiliaryDataLocation :: GetParallelDataResponse -> Lude.Maybe ParallelDataDataLocation) (\s a -> s {latestUpdateAttemptAuxiliaryDataLocation = a} :: GetParallelDataResponse)
{-# DEPRECATED gpdrsLatestUpdateAttemptAuxiliaryDataLocation "Use generic-lens or generic-optics with 'latestUpdateAttemptAuxiliaryDataLocation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrsResponseStatus :: Lens.Lens' GetParallelDataResponse Lude.Int
gpdrsResponseStatus = Lens.lens (responseStatus :: GetParallelDataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetParallelDataResponse)
{-# DEPRECATED gpdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
