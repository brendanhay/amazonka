{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of the filter specified by the filter name.
module Network.AWS.GuardDuty.GetFilter
  ( -- * Creating a request
    GetFilter (..),
    mkGetFilter,

    -- ** Request lenses
    gFilterName,
    gDetectorId,

    -- * Destructuring the response
    GetFilterResponse (..),
    mkGetFilterResponse,

    -- ** Response lenses
    gfrsFindingCriteria,
    gfrsAction,
    gfrsName,
    gfrsDescription,
    gfrsRank,
    gfrsTags,
    gfrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFilter' smart constructor.
data GetFilter = GetFilter'
  { -- | The name of the filter you want to get.
    filterName :: Lude.Text,
    -- | The unique ID of the detector that the filter is associated with.
    detectorId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFilter' with the minimum fields required to make a request.
--
-- * 'filterName' - The name of the filter you want to get.
-- * 'detectorId' - The unique ID of the detector that the filter is associated with.
mkGetFilter ::
  -- | 'filterName'
  Lude.Text ->
  -- | 'detectorId'
  Lude.Text ->
  GetFilter
mkGetFilter pFilterName_ pDetectorId_ =
  GetFilter' {filterName = pFilterName_, detectorId = pDetectorId_}

-- | The name of the filter you want to get.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gFilterName :: Lens.Lens' GetFilter Lude.Text
gFilterName = Lens.lens (filterName :: GetFilter -> Lude.Text) (\s a -> s {filterName = a} :: GetFilter)
{-# DEPRECATED gFilterName "Use generic-lens or generic-optics with 'filterName' instead." #-}

-- | The unique ID of the detector that the filter is associated with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gDetectorId :: Lens.Lens' GetFilter Lude.Text
gDetectorId = Lens.lens (detectorId :: GetFilter -> Lude.Text) (\s a -> s {detectorId = a} :: GetFilter)
{-# DEPRECATED gDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Lude.AWSRequest GetFilter where
  type Rs GetFilter = GetFilterResponse
  request = Req.get guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFilterResponse'
            Lude.<$> (x Lude..:> "findingCriteria")
            Lude.<*> (x Lude..:> "action")
            Lude.<*> (x Lude..:> "name")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (x Lude..?> "rank")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFilter where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetFilter where
  toPath GetFilter' {..} =
    Lude.mconcat
      [ "/detector/",
        Lude.toBS detectorId,
        "/filter/",
        Lude.toBS filterName
      ]

instance Lude.ToQuery GetFilter where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetFilterResponse' smart constructor.
data GetFilterResponse = GetFilterResponse'
  { -- | Represents the criteria to be used in the filter for querying findings.
    findingCriteria :: FindingCriteria,
    -- | Specifies the action that is to be applied to the findings that match the filter.
    action :: FilterAction,
    -- | The name of the filter.
    name :: Lude.Text,
    -- | The description of the filter.
    description :: Lude.Maybe Lude.Text,
    -- | Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
    rank :: Lude.Maybe Lude.Natural,
    -- | The tags of the filter resource.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFilterResponse' with the minimum fields required to make a request.
--
-- * 'findingCriteria' - Represents the criteria to be used in the filter for querying findings.
-- * 'action' - Specifies the action that is to be applied to the findings that match the filter.
-- * 'name' - The name of the filter.
-- * 'description' - The description of the filter.
-- * 'rank' - Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
-- * 'tags' - The tags of the filter resource.
-- * 'responseStatus' - The response status code.
mkGetFilterResponse ::
  -- | 'findingCriteria'
  FindingCriteria ->
  -- | 'action'
  FilterAction ->
  -- | 'name'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  GetFilterResponse
mkGetFilterResponse
  pFindingCriteria_
  pAction_
  pName_
  pResponseStatus_ =
    GetFilterResponse'
      { findingCriteria = pFindingCriteria_,
        action = pAction_,
        name = pName_,
        description = Lude.Nothing,
        rank = Lude.Nothing,
        tags = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Represents the criteria to be used in the filter for querying findings.
--
-- /Note:/ Consider using 'findingCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsFindingCriteria :: Lens.Lens' GetFilterResponse FindingCriteria
gfrsFindingCriteria = Lens.lens (findingCriteria :: GetFilterResponse -> FindingCriteria) (\s a -> s {findingCriteria = a} :: GetFilterResponse)
{-# DEPRECATED gfrsFindingCriteria "Use generic-lens or generic-optics with 'findingCriteria' instead." #-}

-- | Specifies the action that is to be applied to the findings that match the filter.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsAction :: Lens.Lens' GetFilterResponse FilterAction
gfrsAction = Lens.lens (action :: GetFilterResponse -> FilterAction) (\s a -> s {action = a} :: GetFilterResponse)
{-# DEPRECATED gfrsAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The name of the filter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsName :: Lens.Lens' GetFilterResponse Lude.Text
gfrsName = Lens.lens (name :: GetFilterResponse -> Lude.Text) (\s a -> s {name = a} :: GetFilterResponse)
{-# DEPRECATED gfrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The description of the filter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsDescription :: Lens.Lens' GetFilterResponse (Lude.Maybe Lude.Text)
gfrsDescription = Lens.lens (description :: GetFilterResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GetFilterResponse)
{-# DEPRECATED gfrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
--
-- /Note:/ Consider using 'rank' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsRank :: Lens.Lens' GetFilterResponse (Lude.Maybe Lude.Natural)
gfrsRank = Lens.lens (rank :: GetFilterResponse -> Lude.Maybe Lude.Natural) (\s a -> s {rank = a} :: GetFilterResponse)
{-# DEPRECATED gfrsRank "Use generic-lens or generic-optics with 'rank' instead." #-}

-- | The tags of the filter resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsTags :: Lens.Lens' GetFilterResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gfrsTags = Lens.lens (tags :: GetFilterResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: GetFilterResponse)
{-# DEPRECATED gfrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsResponseStatus :: Lens.Lens' GetFilterResponse Lude.Int
gfrsResponseStatus = Lens.lens (responseStatus :: GetFilterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFilterResponse)
{-# DEPRECATED gfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
