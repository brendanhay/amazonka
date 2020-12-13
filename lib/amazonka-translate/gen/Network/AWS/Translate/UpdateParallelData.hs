{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.UpdateParallelData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a previously created parallel data resource by importing a new input file from Amazon S3.
module Network.AWS.Translate.UpdateParallelData
  ( -- * Creating a request
    UpdateParallelData (..),
    mkUpdateParallelData,

    -- ** Request lenses
    updClientToken,
    updName,
    updDescription,
    updParallelDataConfig,

    -- * Destructuring the response
    UpdateParallelDataResponse (..),
    mkUpdateParallelDataResponse,

    -- ** Response lenses
    updrsStatus,
    updrsName,
    updrsLatestUpdateAttemptAt,
    updrsLatestUpdateAttemptStatus,
    updrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Translate.Types

-- | /See:/ 'mkUpdateParallelData' smart constructor.
data UpdateParallelData = UpdateParallelData'
  { -- | A unique identifier for the request. This token is automatically generated when you use Amazon Translate through an AWS SDK.
    clientToken :: Lude.Text,
    -- | The name of the parallel data resource being updated.
    name :: Lude.Text,
    -- | A custom description for the parallel data resource in Amazon Translate.
    description :: Lude.Maybe Lude.Text,
    -- | Specifies the format and S3 location of the parallel data input file.
    parallelDataConfig :: ParallelDataConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateParallelData' with the minimum fields required to make a request.
--
-- * 'clientToken' - A unique identifier for the request. This token is automatically generated when you use Amazon Translate through an AWS SDK.
-- * 'name' - The name of the parallel data resource being updated.
-- * 'description' - A custom description for the parallel data resource in Amazon Translate.
-- * 'parallelDataConfig' - Specifies the format and S3 location of the parallel data input file.
mkUpdateParallelData ::
  -- | 'clientToken'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'parallelDataConfig'
  ParallelDataConfig ->
  UpdateParallelData
mkUpdateParallelData pClientToken_ pName_ pParallelDataConfig_ =
  UpdateParallelData'
    { clientToken = pClientToken_,
      name = pName_,
      description = Lude.Nothing,
      parallelDataConfig = pParallelDataConfig_
    }

-- | A unique identifier for the request. This token is automatically generated when you use Amazon Translate through an AWS SDK.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updClientToken :: Lens.Lens' UpdateParallelData Lude.Text
updClientToken = Lens.lens (clientToken :: UpdateParallelData -> Lude.Text) (\s a -> s {clientToken = a} :: UpdateParallelData)
{-# DEPRECATED updClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The name of the parallel data resource being updated.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updName :: Lens.Lens' UpdateParallelData Lude.Text
updName = Lens.lens (name :: UpdateParallelData -> Lude.Text) (\s a -> s {name = a} :: UpdateParallelData)
{-# DEPRECATED updName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A custom description for the parallel data resource in Amazon Translate.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updDescription :: Lens.Lens' UpdateParallelData (Lude.Maybe Lude.Text)
updDescription = Lens.lens (description :: UpdateParallelData -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateParallelData)
{-# DEPRECATED updDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies the format and S3 location of the parallel data input file.
--
-- /Note:/ Consider using 'parallelDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updParallelDataConfig :: Lens.Lens' UpdateParallelData ParallelDataConfig
updParallelDataConfig = Lens.lens (parallelDataConfig :: UpdateParallelData -> ParallelDataConfig) (\s a -> s {parallelDataConfig = a} :: UpdateParallelData)
{-# DEPRECATED updParallelDataConfig "Use generic-lens or generic-optics with 'parallelDataConfig' instead." #-}

instance Lude.AWSRequest UpdateParallelData where
  type Rs UpdateParallelData = UpdateParallelDataResponse
  request = Req.postJSON translateService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateParallelDataResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "LatestUpdateAttemptAt")
            Lude.<*> (x Lude..?> "LatestUpdateAttemptStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateParallelData where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShineFrontendService_20170701.UpdateParallelData" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateParallelData where
  toJSON UpdateParallelData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ClientToken" Lude..= clientToken),
            Lude.Just ("Name" Lude..= name),
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("ParallelDataConfig" Lude..= parallelDataConfig)
          ]
      )

instance Lude.ToPath UpdateParallelData where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateParallelData where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateParallelDataResponse' smart constructor.
data UpdateParallelDataResponse = UpdateParallelDataResponse'
  { -- | The status of the parallel data resource that you are attempting to update. Your update request is accepted only if this status is either @ACTIVE@ or @FAILED@ .
    status :: Lude.Maybe ParallelDataStatus,
    -- | The name of the parallel data resource being updated.
    name :: Lude.Maybe Lude.Text,
    -- | The time that the most recent update was attempted.
    latestUpdateAttemptAt :: Lude.Maybe Lude.Timestamp,
    -- | The status of the parallel data update attempt. When the updated parallel data resource is ready for you to use, the status is @ACTIVE@ .
    latestUpdateAttemptStatus :: Lude.Maybe ParallelDataStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateParallelDataResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the parallel data resource that you are attempting to update. Your update request is accepted only if this status is either @ACTIVE@ or @FAILED@ .
-- * 'name' - The name of the parallel data resource being updated.
-- * 'latestUpdateAttemptAt' - The time that the most recent update was attempted.
-- * 'latestUpdateAttemptStatus' - The status of the parallel data update attempt. When the updated parallel data resource is ready for you to use, the status is @ACTIVE@ .
-- * 'responseStatus' - The response status code.
mkUpdateParallelDataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateParallelDataResponse
mkUpdateParallelDataResponse pResponseStatus_ =
  UpdateParallelDataResponse'
    { status = Lude.Nothing,
      name = Lude.Nothing,
      latestUpdateAttemptAt = Lude.Nothing,
      latestUpdateAttemptStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the parallel data resource that you are attempting to update. Your update request is accepted only if this status is either @ACTIVE@ or @FAILED@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updrsStatus :: Lens.Lens' UpdateParallelDataResponse (Lude.Maybe ParallelDataStatus)
updrsStatus = Lens.lens (status :: UpdateParallelDataResponse -> Lude.Maybe ParallelDataStatus) (\s a -> s {status = a} :: UpdateParallelDataResponse)
{-# DEPRECATED updrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the parallel data resource being updated.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updrsName :: Lens.Lens' UpdateParallelDataResponse (Lude.Maybe Lude.Text)
updrsName = Lens.lens (name :: UpdateParallelDataResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateParallelDataResponse)
{-# DEPRECATED updrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time that the most recent update was attempted.
--
-- /Note:/ Consider using 'latestUpdateAttemptAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updrsLatestUpdateAttemptAt :: Lens.Lens' UpdateParallelDataResponse (Lude.Maybe Lude.Timestamp)
updrsLatestUpdateAttemptAt = Lens.lens (latestUpdateAttemptAt :: UpdateParallelDataResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {latestUpdateAttemptAt = a} :: UpdateParallelDataResponse)
{-# DEPRECATED updrsLatestUpdateAttemptAt "Use generic-lens or generic-optics with 'latestUpdateAttemptAt' instead." #-}

-- | The status of the parallel data update attempt. When the updated parallel data resource is ready for you to use, the status is @ACTIVE@ .
--
-- /Note:/ Consider using 'latestUpdateAttemptStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updrsLatestUpdateAttemptStatus :: Lens.Lens' UpdateParallelDataResponse (Lude.Maybe ParallelDataStatus)
updrsLatestUpdateAttemptStatus = Lens.lens (latestUpdateAttemptStatus :: UpdateParallelDataResponse -> Lude.Maybe ParallelDataStatus) (\s a -> s {latestUpdateAttemptStatus = a} :: UpdateParallelDataResponse)
{-# DEPRECATED updrsLatestUpdateAttemptStatus "Use generic-lens or generic-optics with 'latestUpdateAttemptStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updrsResponseStatus :: Lens.Lens' UpdateParallelDataResponse Lude.Int
updrsResponseStatus = Lens.lens (responseStatus :: UpdateParallelDataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateParallelDataResponse)
{-# DEPRECATED updrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
