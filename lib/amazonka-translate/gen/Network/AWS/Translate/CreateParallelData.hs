{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.CreateParallelData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a parallel data resource in Amazon Translate by importing an input file from Amazon S3. Parallel data files contain examples of source phrases and their translations from your translation memory. By adding parallel data, you can influence the style, tone, and word choice in your translation output.
module Network.AWS.Translate.CreateParallelData
  ( -- * Creating a request
    CreateParallelData (..),
    mkCreateParallelData,

    -- ** Request lenses
    cpdClientToken,
    cpdName,
    cpdEncryptionKey,
    cpdDescription,
    cpdParallelDataConfig,

    -- * Destructuring the response
    CreateParallelDataResponse (..),
    mkCreateParallelDataResponse,

    -- ** Response lenses
    cpdrsStatus,
    cpdrsName,
    cpdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Translate.Types

-- | /See:/ 'mkCreateParallelData' smart constructor.
data CreateParallelData = CreateParallelData'
  { -- | A unique identifier for the request. This token is automatically generated when you use Amazon Translate through an AWS SDK.
    clientToken :: Lude.Text,
    -- | A custom name for the parallel data resource in Amazon Translate. You must assign a name that is unique in the account and region.
    name :: Lude.Text,
    encryptionKey :: Lude.Maybe EncryptionKey,
    -- | A custom description for the parallel data resource in Amazon Translate.
    description :: Lude.Maybe Lude.Text,
    -- | Specifies the format and S3 location of the parallel data input file.
    parallelDataConfig :: ParallelDataConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateParallelData' with the minimum fields required to make a request.
--
-- * 'clientToken' - A unique identifier for the request. This token is automatically generated when you use Amazon Translate through an AWS SDK.
-- * 'name' - A custom name for the parallel data resource in Amazon Translate. You must assign a name that is unique in the account and region.
-- * 'encryptionKey' -
-- * 'description' - A custom description for the parallel data resource in Amazon Translate.
-- * 'parallelDataConfig' - Specifies the format and S3 location of the parallel data input file.
mkCreateParallelData ::
  -- | 'clientToken'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'parallelDataConfig'
  ParallelDataConfig ->
  CreateParallelData
mkCreateParallelData pClientToken_ pName_ pParallelDataConfig_ =
  CreateParallelData'
    { clientToken = pClientToken_,
      name = pName_,
      encryptionKey = Lude.Nothing,
      description = Lude.Nothing,
      parallelDataConfig = pParallelDataConfig_
    }

-- | A unique identifier for the request. This token is automatically generated when you use Amazon Translate through an AWS SDK.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdClientToken :: Lens.Lens' CreateParallelData Lude.Text
cpdClientToken = Lens.lens (clientToken :: CreateParallelData -> Lude.Text) (\s a -> s {clientToken = a} :: CreateParallelData)
{-# DEPRECATED cpdClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | A custom name for the parallel data resource in Amazon Translate. You must assign a name that is unique in the account and region.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdName :: Lens.Lens' CreateParallelData Lude.Text
cpdName = Lens.lens (name :: CreateParallelData -> Lude.Text) (\s a -> s {name = a} :: CreateParallelData)
{-# DEPRECATED cpdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdEncryptionKey :: Lens.Lens' CreateParallelData (Lude.Maybe EncryptionKey)
cpdEncryptionKey = Lens.lens (encryptionKey :: CreateParallelData -> Lude.Maybe EncryptionKey) (\s a -> s {encryptionKey = a} :: CreateParallelData)
{-# DEPRECATED cpdEncryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead." #-}

-- | A custom description for the parallel data resource in Amazon Translate.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdDescription :: Lens.Lens' CreateParallelData (Lude.Maybe Lude.Text)
cpdDescription = Lens.lens (description :: CreateParallelData -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateParallelData)
{-# DEPRECATED cpdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies the format and S3 location of the parallel data input file.
--
-- /Note:/ Consider using 'parallelDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdParallelDataConfig :: Lens.Lens' CreateParallelData ParallelDataConfig
cpdParallelDataConfig = Lens.lens (parallelDataConfig :: CreateParallelData -> ParallelDataConfig) (\s a -> s {parallelDataConfig = a} :: CreateParallelData)
{-# DEPRECATED cpdParallelDataConfig "Use generic-lens or generic-optics with 'parallelDataConfig' instead." #-}

instance Lude.AWSRequest CreateParallelData where
  type Rs CreateParallelData = CreateParallelDataResponse
  request = Req.postJSON translateService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateParallelDataResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateParallelData where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShineFrontendService_20170701.CreateParallelData" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateParallelData where
  toJSON CreateParallelData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ClientToken" Lude..= clientToken),
            Lude.Just ("Name" Lude..= name),
            ("EncryptionKey" Lude..=) Lude.<$> encryptionKey,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("ParallelDataConfig" Lude..= parallelDataConfig)
          ]
      )

instance Lude.ToPath CreateParallelData where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateParallelData where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateParallelDataResponse' smart constructor.
data CreateParallelDataResponse = CreateParallelDataResponse'
  { -- | The status of the parallel data resource. When the resource is ready for you to use, the status is @ACTIVE@ .
    status :: Lude.Maybe ParallelDataStatus,
    -- | The custom name that you assigned to the parallel data resource.
    name :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateParallelDataResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the parallel data resource. When the resource is ready for you to use, the status is @ACTIVE@ .
-- * 'name' - The custom name that you assigned to the parallel data resource.
-- * 'responseStatus' - The response status code.
mkCreateParallelDataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateParallelDataResponse
mkCreateParallelDataResponse pResponseStatus_ =
  CreateParallelDataResponse'
    { status = Lude.Nothing,
      name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the parallel data resource. When the resource is ready for you to use, the status is @ACTIVE@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdrsStatus :: Lens.Lens' CreateParallelDataResponse (Lude.Maybe ParallelDataStatus)
cpdrsStatus = Lens.lens (status :: CreateParallelDataResponse -> Lude.Maybe ParallelDataStatus) (\s a -> s {status = a} :: CreateParallelDataResponse)
{-# DEPRECATED cpdrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The custom name that you assigned to the parallel data resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdrsName :: Lens.Lens' CreateParallelDataResponse (Lude.Maybe Lude.Text)
cpdrsName = Lens.lens (name :: CreateParallelDataResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateParallelDataResponse)
{-# DEPRECATED cpdrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdrsResponseStatus :: Lens.Lens' CreateParallelDataResponse Lude.Int
cpdrsResponseStatus = Lens.lens (responseStatus :: CreateParallelDataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateParallelDataResponse)
{-# DEPRECATED cpdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
