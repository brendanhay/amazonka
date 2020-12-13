{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.CreateDetector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a single Amazon GuardDuty detector. A detector is a resource that represents the GuardDuty service. To start using GuardDuty, you must create a detector in each Region where you enable the service. You can have only one detector per account per Region. All data sources are enabled in a new detector by default.
module Network.AWS.GuardDuty.CreateDetector
  ( -- * Creating a request
    CreateDetector (..),
    mkCreateDetector,

    -- ** Request lenses
    cdClientToken,
    cdFindingPublishingFrequency,
    cdDataSources,
    cdEnable,
    cdTags,

    -- * Destructuring the response
    CreateDetectorResponse (..),
    mkCreateDetectorResponse,

    -- ** Response lenses
    cdrsDetectorId,
    cdrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDetector' smart constructor.
data CreateDetector = CreateDetector'
  { -- | The idempotency token for the create request.
    clientToken :: Lude.Maybe Lude.Text,
    -- | An enum value that specifies how frequently updated findings are exported.
    findingPublishingFrequency :: Lude.Maybe FindingPublishingFrequency,
    -- | An object that describes which data sources will be enabled for the detector.
    dataSources :: Lude.Maybe DataSourceConfigurations,
    -- | A Boolean value that specifies whether the detector is to be enabled.
    enable :: Lude.Bool,
    -- | The tags to be added to a new detector resource.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDetector' with the minimum fields required to make a request.
--
-- * 'clientToken' - The idempotency token for the create request.
-- * 'findingPublishingFrequency' - An enum value that specifies how frequently updated findings are exported.
-- * 'dataSources' - An object that describes which data sources will be enabled for the detector.
-- * 'enable' - A Boolean value that specifies whether the detector is to be enabled.
-- * 'tags' - The tags to be added to a new detector resource.
mkCreateDetector ::
  -- | 'enable'
  Lude.Bool ->
  CreateDetector
mkCreateDetector pEnable_ =
  CreateDetector'
    { clientToken = Lude.Nothing,
      findingPublishingFrequency = Lude.Nothing,
      dataSources = Lude.Nothing,
      enable = pEnable_,
      tags = Lude.Nothing
    }

-- | The idempotency token for the create request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdClientToken :: Lens.Lens' CreateDetector (Lude.Maybe Lude.Text)
cdClientToken = Lens.lens (clientToken :: CreateDetector -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateDetector)
{-# DEPRECATED cdClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | An enum value that specifies how frequently updated findings are exported.
--
-- /Note:/ Consider using 'findingPublishingFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdFindingPublishingFrequency :: Lens.Lens' CreateDetector (Lude.Maybe FindingPublishingFrequency)
cdFindingPublishingFrequency = Lens.lens (findingPublishingFrequency :: CreateDetector -> Lude.Maybe FindingPublishingFrequency) (\s a -> s {findingPublishingFrequency = a} :: CreateDetector)
{-# DEPRECATED cdFindingPublishingFrequency "Use generic-lens or generic-optics with 'findingPublishingFrequency' instead." #-}

-- | An object that describes which data sources will be enabled for the detector.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDataSources :: Lens.Lens' CreateDetector (Lude.Maybe DataSourceConfigurations)
cdDataSources = Lens.lens (dataSources :: CreateDetector -> Lude.Maybe DataSourceConfigurations) (\s a -> s {dataSources = a} :: CreateDetector)
{-# DEPRECATED cdDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

-- | A Boolean value that specifies whether the detector is to be enabled.
--
-- /Note:/ Consider using 'enable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdEnable :: Lens.Lens' CreateDetector Lude.Bool
cdEnable = Lens.lens (enable :: CreateDetector -> Lude.Bool) (\s a -> s {enable = a} :: CreateDetector)
{-# DEPRECATED cdEnable "Use generic-lens or generic-optics with 'enable' instead." #-}

-- | The tags to be added to a new detector resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTags :: Lens.Lens' CreateDetector (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cdTags = Lens.lens (tags :: CreateDetector -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateDetector)
{-# DEPRECATED cdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateDetector where
  type Rs CreateDetector = CreateDetectorResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDetectorResponse'
            Lude.<$> (x Lude..?> "detectorId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDetector where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDetector where
  toJSON CreateDetector' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("clientToken" Lude..=) Lude.<$> clientToken,
            ("findingPublishingFrequency" Lude..=)
              Lude.<$> findingPublishingFrequency,
            ("dataSources" Lude..=) Lude.<$> dataSources,
            Lude.Just ("enable" Lude..= enable),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateDetector where
  toPath = Lude.const "/detector"

instance Lude.ToQuery CreateDetector where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDetectorResponse' smart constructor.
data CreateDetectorResponse = CreateDetectorResponse'
  { -- | The unique ID of the created detector.
    detectorId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDetectorResponse' with the minimum fields required to make a request.
--
-- * 'detectorId' - The unique ID of the created detector.
-- * 'responseStatus' - The response status code.
mkCreateDetectorResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDetectorResponse
mkCreateDetectorResponse pResponseStatus_ =
  CreateDetectorResponse'
    { detectorId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique ID of the created detector.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsDetectorId :: Lens.Lens' CreateDetectorResponse (Lude.Maybe Lude.Text)
cdrsDetectorId = Lens.lens (detectorId :: CreateDetectorResponse -> Lude.Maybe Lude.Text) (\s a -> s {detectorId = a} :: CreateDetectorResponse)
{-# DEPRECATED cdrsDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsResponseStatus :: Lens.Lens' CreateDetectorResponse Lude.Int
cdrsResponseStatus = Lens.lens (responseStatus :: CreateDetectorResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDetectorResponse)
{-# DEPRECATED cdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
