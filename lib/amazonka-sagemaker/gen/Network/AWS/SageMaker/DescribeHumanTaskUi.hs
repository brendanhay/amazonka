{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dhtufrsCreationTime,
    dhtufrsUiTemplate,
    dhtufrsHumanTaskUiName,
    dhtufrsHumanTaskUiStatus,
    dhtufrsHumanTaskUiARN,
    dhtufrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeHumanTaskUi' smart constructor.
newtype DescribeHumanTaskUi = DescribeHumanTaskUi'
  { -- | The name of the human task user interface (worker task template) you want information about.
    humanTaskUiName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
            Lude.<$> (x Lude..:> "CreationTime")
            Lude.<*> (x Lude..:> "UiTemplate")
            Lude.<*> (x Lude..:> "HumanTaskUiName")
            Lude.<*> (x Lude..?> "HumanTaskUiStatus")
            Lude.<*> (x Lude..:> "HumanTaskUiArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
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
  { -- | The timestamp when the human task user interface was created.
    creationTime :: Lude.Timestamp,
    uiTemplate :: UiTemplateInfo,
    -- | The name of the human task user interface (worker task template).
    humanTaskUiName :: Lude.Text,
    -- | The status of the human task user interface (worker task template). Valid values are listed below.
    humanTaskUiStatus :: Lude.Maybe HumanTaskUiStatus,
    -- | The Amazon Resource Name (ARN) of the human task user interface (worker task template).
    humanTaskUiARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHumanTaskUiResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - The timestamp when the human task user interface was created.
-- * 'uiTemplate' -
-- * 'humanTaskUiName' - The name of the human task user interface (worker task template).
-- * 'humanTaskUiStatus' - The status of the human task user interface (worker task template). Valid values are listed below.
-- * 'humanTaskUiARN' - The Amazon Resource Name (ARN) of the human task user interface (worker task template).
-- * 'responseStatus' - The response status code.
mkDescribeHumanTaskUiResponse ::
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'uiTemplate'
  UiTemplateInfo ->
  -- | 'humanTaskUiName'
  Lude.Text ->
  -- | 'humanTaskUiARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeHumanTaskUiResponse
mkDescribeHumanTaskUiResponse
  pCreationTime_
  pUiTemplate_
  pHumanTaskUiName_
  pHumanTaskUiARN_
  pResponseStatus_ =
    DescribeHumanTaskUiResponse'
      { creationTime = pCreationTime_,
        uiTemplate = pUiTemplate_,
        humanTaskUiName = pHumanTaskUiName_,
        humanTaskUiStatus = Lude.Nothing,
        humanTaskUiARN = pHumanTaskUiARN_,
        responseStatus = pResponseStatus_
      }

-- | The timestamp when the human task user interface was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhtufrsCreationTime :: Lens.Lens' DescribeHumanTaskUiResponse Lude.Timestamp
dhtufrsCreationTime = Lens.lens (creationTime :: DescribeHumanTaskUiResponse -> Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeHumanTaskUiResponse)
{-# DEPRECATED dhtufrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'uiTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhtufrsUiTemplate :: Lens.Lens' DescribeHumanTaskUiResponse UiTemplateInfo
dhtufrsUiTemplate = Lens.lens (uiTemplate :: DescribeHumanTaskUiResponse -> UiTemplateInfo) (\s a -> s {uiTemplate = a} :: DescribeHumanTaskUiResponse)
{-# DEPRECATED dhtufrsUiTemplate "Use generic-lens or generic-optics with 'uiTemplate' instead." #-}

-- | The name of the human task user interface (worker task template).
--
-- /Note:/ Consider using 'humanTaskUiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhtufrsHumanTaskUiName :: Lens.Lens' DescribeHumanTaskUiResponse Lude.Text
dhtufrsHumanTaskUiName = Lens.lens (humanTaskUiName :: DescribeHumanTaskUiResponse -> Lude.Text) (\s a -> s {humanTaskUiName = a} :: DescribeHumanTaskUiResponse)
{-# DEPRECATED dhtufrsHumanTaskUiName "Use generic-lens or generic-optics with 'humanTaskUiName' instead." #-}

-- | The status of the human task user interface (worker task template). Valid values are listed below.
--
-- /Note:/ Consider using 'humanTaskUiStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhtufrsHumanTaskUiStatus :: Lens.Lens' DescribeHumanTaskUiResponse (Lude.Maybe HumanTaskUiStatus)
dhtufrsHumanTaskUiStatus = Lens.lens (humanTaskUiStatus :: DescribeHumanTaskUiResponse -> Lude.Maybe HumanTaskUiStatus) (\s a -> s {humanTaskUiStatus = a} :: DescribeHumanTaskUiResponse)
{-# DEPRECATED dhtufrsHumanTaskUiStatus "Use generic-lens or generic-optics with 'humanTaskUiStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the human task user interface (worker task template).
--
-- /Note:/ Consider using 'humanTaskUiARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhtufrsHumanTaskUiARN :: Lens.Lens' DescribeHumanTaskUiResponse Lude.Text
dhtufrsHumanTaskUiARN = Lens.lens (humanTaskUiARN :: DescribeHumanTaskUiResponse -> Lude.Text) (\s a -> s {humanTaskUiARN = a} :: DescribeHumanTaskUiResponse)
{-# DEPRECATED dhtufrsHumanTaskUiARN "Use generic-lens or generic-optics with 'humanTaskUiARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhtufrsResponseStatus :: Lens.Lens' DescribeHumanTaskUiResponse Lude.Int
dhtufrsResponseStatus = Lens.lens (responseStatus :: DescribeHumanTaskUiResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeHumanTaskUiResponse)
{-# DEPRECATED dhtufrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
