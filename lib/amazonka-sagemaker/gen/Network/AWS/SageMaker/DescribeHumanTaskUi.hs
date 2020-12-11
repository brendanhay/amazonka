{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeHumanTaskUi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the requested human task user interface (worker task template).
module Network.AWS.SageMaker.DescribeHumanTaskUi
  ( -- * Creating a request
    DescribeHumanTaskUi (..),
    mkDescribeHumanTaskUi,

    -- ** Request lenses
    dHumanTaskUiName,

    -- * Destructuring the response
    DescribeHumanTaskUiResponse (..),
    mkDescribeHumanTaskUiResponse,

    -- ** Response lenses
    dhtuhrsHumanTaskUiStatus,
    dhtuhrsResponseStatus,
    dhtuhrsHumanTaskUiARN,
    dhtuhrsHumanTaskUiName,
    dhtuhrsCreationTime,
    dhtuhrsUiTemplate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeHumanTaskUi' smart constructor.
newtype DescribeHumanTaskUi = DescribeHumanTaskUi'
  { humanTaskUiName ::
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

-- | Creates a value of 'DescribeHumanTaskUi' with the minimum fields required to make a request.
--
-- * 'humanTaskUiName' - The name of the human task user interface (worker task template) you want information about.
mkDescribeHumanTaskUi ::
  -- | 'humanTaskUiName'
  Lude.Text ->
  DescribeHumanTaskUi
mkDescribeHumanTaskUi pHumanTaskUiName_ =
  DescribeHumanTaskUi' {humanTaskUiName = pHumanTaskUiName_}

-- | The name of the human task user interface (worker task template) you want information about.
--
-- /Note:/ Consider using 'humanTaskUiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHumanTaskUiName :: Lens.Lens' DescribeHumanTaskUi Lude.Text
dHumanTaskUiName = Lens.lens (humanTaskUiName :: DescribeHumanTaskUi -> Lude.Text) (\s a -> s {humanTaskUiName = a} :: DescribeHumanTaskUi)
{-# DEPRECATED dHumanTaskUiName "Use generic-lens or generic-optics with 'humanTaskUiName' instead." #-}

instance Lude.AWSRequest DescribeHumanTaskUi where
  type Rs DescribeHumanTaskUi = DescribeHumanTaskUiResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeHumanTaskUiResponse'
            Lude.<$> (x Lude..?> "HumanTaskUiStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "HumanTaskUiArn")
            Lude.<*> (x Lude..:> "HumanTaskUiName")
            Lude.<*> (x Lude..:> "CreationTime")
            Lude.<*> (x Lude..:> "UiTemplate")
      )

instance Lude.ToHeaders DescribeHumanTaskUi where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeHumanTaskUi" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeHumanTaskUi where
  toJSON DescribeHumanTaskUi' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("HumanTaskUiName" Lude..= humanTaskUiName)]
      )

instance Lude.ToPath DescribeHumanTaskUi where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeHumanTaskUi where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeHumanTaskUiResponse' smart constructor.
data DescribeHumanTaskUiResponse = DescribeHumanTaskUiResponse'
  { humanTaskUiStatus ::
      Lude.Maybe HumanTaskUiStatus,
    responseStatus :: Lude.Int,
    humanTaskUiARN :: Lude.Text,
    humanTaskUiName :: Lude.Text,
    creationTime :: Lude.Timestamp,
    uiTemplate :: UiTemplateInfo
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHumanTaskUiResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - The timestamp when the human task user interface was created.
-- * 'humanTaskUiARN' - The Amazon Resource Name (ARN) of the human task user interface (worker task template).
-- * 'humanTaskUiName' - The name of the human task user interface (worker task template).
-- * 'humanTaskUiStatus' - The status of the human task user interface (worker task template). Valid values are listed below.
-- * 'responseStatus' - The response status code.
-- * 'uiTemplate' - Undocumented field.
mkDescribeHumanTaskUiResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'humanTaskUiARN'
  Lude.Text ->
  -- | 'humanTaskUiName'
  Lude.Text ->
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'uiTemplate'
  UiTemplateInfo ->
  DescribeHumanTaskUiResponse
mkDescribeHumanTaskUiResponse
  pResponseStatus_
  pHumanTaskUiARN_
  pHumanTaskUiName_
  pCreationTime_
  pUiTemplate_ =
    DescribeHumanTaskUiResponse'
      { humanTaskUiStatus = Lude.Nothing,
        responseStatus = pResponseStatus_,
        humanTaskUiARN = pHumanTaskUiARN_,
        humanTaskUiName = pHumanTaskUiName_,
        creationTime = pCreationTime_,
        uiTemplate = pUiTemplate_
      }

-- | The status of the human task user interface (worker task template). Valid values are listed below.
--
-- /Note:/ Consider using 'humanTaskUiStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhtuhrsHumanTaskUiStatus :: Lens.Lens' DescribeHumanTaskUiResponse (Lude.Maybe HumanTaskUiStatus)
dhtuhrsHumanTaskUiStatus = Lens.lens (humanTaskUiStatus :: DescribeHumanTaskUiResponse -> Lude.Maybe HumanTaskUiStatus) (\s a -> s {humanTaskUiStatus = a} :: DescribeHumanTaskUiResponse)
{-# DEPRECATED dhtuhrsHumanTaskUiStatus "Use generic-lens or generic-optics with 'humanTaskUiStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhtuhrsResponseStatus :: Lens.Lens' DescribeHumanTaskUiResponse Lude.Int
dhtuhrsResponseStatus = Lens.lens (responseStatus :: DescribeHumanTaskUiResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeHumanTaskUiResponse)
{-# DEPRECATED dhtuhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the human task user interface (worker task template).
--
-- /Note:/ Consider using 'humanTaskUiARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhtuhrsHumanTaskUiARN :: Lens.Lens' DescribeHumanTaskUiResponse Lude.Text
dhtuhrsHumanTaskUiARN = Lens.lens (humanTaskUiARN :: DescribeHumanTaskUiResponse -> Lude.Text) (\s a -> s {humanTaskUiARN = a} :: DescribeHumanTaskUiResponse)
{-# DEPRECATED dhtuhrsHumanTaskUiARN "Use generic-lens or generic-optics with 'humanTaskUiARN' instead." #-}

-- | The name of the human task user interface (worker task template).
--
-- /Note:/ Consider using 'humanTaskUiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhtuhrsHumanTaskUiName :: Lens.Lens' DescribeHumanTaskUiResponse Lude.Text
dhtuhrsHumanTaskUiName = Lens.lens (humanTaskUiName :: DescribeHumanTaskUiResponse -> Lude.Text) (\s a -> s {humanTaskUiName = a} :: DescribeHumanTaskUiResponse)
{-# DEPRECATED dhtuhrsHumanTaskUiName "Use generic-lens or generic-optics with 'humanTaskUiName' instead." #-}

-- | The timestamp when the human task user interface was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhtuhrsCreationTime :: Lens.Lens' DescribeHumanTaskUiResponse Lude.Timestamp
dhtuhrsCreationTime = Lens.lens (creationTime :: DescribeHumanTaskUiResponse -> Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeHumanTaskUiResponse)
{-# DEPRECATED dhtuhrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'uiTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhtuhrsUiTemplate :: Lens.Lens' DescribeHumanTaskUiResponse UiTemplateInfo
dhtuhrsUiTemplate = Lens.lens (uiTemplate :: DescribeHumanTaskUiResponse -> UiTemplateInfo) (\s a -> s {uiTemplate = a} :: DescribeHumanTaskUiResponse)
{-# DEPRECATED dhtuhrsUiTemplate "Use generic-lens or generic-optics with 'uiTemplate' instead." #-}
