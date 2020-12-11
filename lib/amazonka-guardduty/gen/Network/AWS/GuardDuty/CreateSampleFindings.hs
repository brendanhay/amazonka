{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.CreateSampleFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates example findings of types specified by the list of finding types. If 'NULL' is specified for @findingTypes@ , the API generates example findings of all supported finding types.
module Network.AWS.GuardDuty.CreateSampleFindings
  ( -- * Creating a request
    CreateSampleFindings (..),
    mkCreateSampleFindings,

    -- ** Request lenses
    csfFindingTypes,
    csfDetectorId,

    -- * Destructuring the response
    CreateSampleFindingsResponse (..),
    mkCreateSampleFindingsResponse,

    -- ** Response lenses
    csfrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSampleFindings' smart constructor.
data CreateSampleFindings = CreateSampleFindings'
  { findingTypes ::
      Lude.Maybe [Lude.Text],
    detectorId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSampleFindings' with the minimum fields required to make a request.
--
-- * 'detectorId' - The ID of the detector to create sample findings for.
-- * 'findingTypes' - The types of sample findings to generate.
mkCreateSampleFindings ::
  -- | 'detectorId'
  Lude.Text ->
  CreateSampleFindings
mkCreateSampleFindings pDetectorId_ =
  CreateSampleFindings'
    { findingTypes = Lude.Nothing,
      detectorId = pDetectorId_
    }

-- | The types of sample findings to generate.
--
-- /Note:/ Consider using 'findingTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfFindingTypes :: Lens.Lens' CreateSampleFindings (Lude.Maybe [Lude.Text])
csfFindingTypes = Lens.lens (findingTypes :: CreateSampleFindings -> Lude.Maybe [Lude.Text]) (\s a -> s {findingTypes = a} :: CreateSampleFindings)
{-# DEPRECATED csfFindingTypes "Use generic-lens or generic-optics with 'findingTypes' instead." #-}

-- | The ID of the detector to create sample findings for.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfDetectorId :: Lens.Lens' CreateSampleFindings Lude.Text
csfDetectorId = Lens.lens (detectorId :: CreateSampleFindings -> Lude.Text) (\s a -> s {detectorId = a} :: CreateSampleFindings)
{-# DEPRECATED csfDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Lude.AWSRequest CreateSampleFindings where
  type Rs CreateSampleFindings = CreateSampleFindingsResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateSampleFindingsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSampleFindings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateSampleFindings where
  toJSON CreateSampleFindings' {..} =
    Lude.object
      (Lude.catMaybes [("findingTypes" Lude..=) Lude.<$> findingTypes])

instance Lude.ToPath CreateSampleFindings where
  toPath CreateSampleFindings' {..} =
    Lude.mconcat
      ["/detector/", Lude.toBS detectorId, "/findings/create"]

instance Lude.ToQuery CreateSampleFindings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateSampleFindingsResponse' smart constructor.
newtype CreateSampleFindingsResponse = CreateSampleFindingsResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSampleFindingsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateSampleFindingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSampleFindingsResponse
mkCreateSampleFindingsResponse pResponseStatus_ =
  CreateSampleFindingsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrsResponseStatus :: Lens.Lens' CreateSampleFindingsResponse Lude.Int
csfrsResponseStatus = Lens.lens (responseStatus :: CreateSampleFindingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSampleFindingsResponse)
{-# DEPRECATED csfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
